package net.maryknollrad.ctdose

import org.ekrich.config.* 
import java.io.File
import scala.util.Try
import scala.jdk.CollectionConverters.*
import cats.data.* 
import cats.data.Validated.*
import cats.effect.IO
import cats.syntax.all.*
import java.time.{LocalDate, LocalDateTime}
import CTDoseInfo.*
import org.http4s.Request

object Configuration:
    case class ConnectionInfo(callingAe: String, calledAe: String, host: String, port: Int, encoding: String) 
    type TesseractPath = String
    case class CTDoseConfig(connectionInfo: ConnectionInfo, db: DB, tpath: TesseractPath, doseDLP: Boolean, institution: List[String], storepng: Option[String], encoding: String,
            processBegin: Option[LocalDate], processDayBehind: Int, pauseInterval: Int, calendarEvent: Option[String], webPort: Option[Int], drlEditIps: List[String], printIp: Boolean):
        def drlEditable(req: Request[_]): Boolean = 
            if printIp then println(s"[${LocalDateTime.now()}] Request from '${req.remoteAddr.map(_.toString).getOrElse("No IP address")}'")
            req.remoteAddr.map(ip => drlEditIps.isEmpty || drlEditIps.contains(ip.toString)).getOrElse(false)

    def ctInfo() = 
        val ctConf = Try(ConfigFactory.load("ct-info").getConfig("CTINFO"))
                        .toEither.left.map(e => List(s"Can't find ct-info.conf file or Config does not have CTINFO ${e.getMessage()}"))
        ctConf.flatMap(conf =>
                val (convergedMap, errs) = conf.entrySet.asScala.foldLeft((Map.empty[(String, String), CTDoseSeriesInfo], List.empty[String]))({ case ((dim, errs), e) =>
                    val ks = e.getKey().split('.').map(_.trim)
                    if ks.length == 3 then
                        val (manu, mod) = (ks(0).filter(_ != '"'), ks(1).filter(_ != '"'))
                        val di = dim.getOrElse((manu, mod), CTDoseSeriesInfo(manu, mod, CTDoseInfo.DoseSeriesInfo()))
                        ks(2) match 
                            case DoseBy =>
                                val v = e.getValue().unwrapped.asInstanceOf[String]
                                val by = v match 
                                    case "number" => Some(CTDoseSeries.SeriesNumber)
                                    case "index" => Some(CTDoseSeries.SeriesIndex)
                                    case "name" => Some(CTDoseSeries.SeriesName)
                                    case _ => None
                                if by.nonEmpty then
                                    (dim.updated((manu, mod), di.copy(seriesInfo=di.seriesInfo.copy(by = by))), errs)
                                else 
                                    (dim, s"Unknown value given to $manu.$mod.$DoseBy : $v" :: errs)
                            case DoseValue =>
                                e.getValue().valueType match
                                    case ConfigValueType.STRING =>
                                        val v = e.getValue().unwrapped.asInstanceOf[String]
                                        (dim.updated((manu, mod), di.copy(seriesInfo=di.seriesInfo.copy(value = Some(v)))), errs)
                                    case ConfigValueType.NUMBER =>
                                        val v = e.getValue().unwrapped.asInstanceOf[Int]
                                        (dim.updated((manu, mod), di.copy(seriesInfo=di.seriesInfo.copy(value = Some(v)))), errs)
                                    case _ =>
                                        (dim, s"Wrong type of value given $manu.$mod.$DoseValue" ::  errs)
                            case ExtReg =>
                                val v = e.getValue().unwrapped.asInstanceOf[String]
                                (dim.updated((manu, mod), di.copy(seriesInfo=di.seriesInfo.copy(regex = Some(v)))), errs)
                            case _ =>
                                (dim, s"Unknown dose series information ${ks(2)}, shoulbe be one of $DoseBy, $DoseValue, $ExtReg" :: errs)
                    else
                        (dim, s"Wrong entry format : ${e.getKey()}, should be MANUFACTURER.MODEL_NAME.[$DoseBy, $DoseValue, $ExtReg]" :: errs)
                })
                val dis = convergedMap.values.toList
                val nerrs = errs ++ dis.filter(!_.seriesInfo.allNonEmpty()).map(di => s"Missing information of ${di.manufacturer}.${di.model} : ${di.seriesInfo.missingField().mkString(",")}")
                    ++ dis.flatMap(di => 
                        (di.seriesInfo.by, di.seriesInfo.value) match 
                            case (Some(CTDoseSeries.SeriesName), Some(_: Int)) =>
                                Some(s"Number value given to dose-series-by: name, ${di.manufacturer}.${di.model}")
                            case (Some(CTDoseSeries.SeriesIndex), Some(_: String)) | (Some(CTDoseSeries.SeriesNumber), Some(_: String)) =>
                                Some(s"String value given, expects Number value, ${di.manufacturer}.${di.model}")
                            case _ =>
                                None)
                if nerrs.isEmpty then
                    convergedMap.asRight[List[String]]
                else
                    nerrs.asLeft[Map[(String, String), CTDoseSeriesInfo]]
            )

    def apply(fname: String = "yader"): Either[String, CTDoseConfig] =
        val ifile = File(s"$fname.conf")
        if ifile.exists() then 
            Try:
                val c = ConfigFactory.parseFile(ifile)
                val ci = ConnectionInfo(
                    c.getString("calling-ae"),
                    c.getString("called-ae"),
                    c.getString("host"),
                    c.getInt("port"),
                    c.getString("encoding")
                )
                def getOptionalString(path: String) = if c.hasPath(path) then Some(c.getString(path)) else None
                def getOptionalInt(path: String) = if c.hasPath(path) then Some(c.getInt(path)) else None
                val tpath = c.getString("tesseract-path")
                val isDLP = c.getBoolean("doseDLP")
                val institutionNames = c.getStringList("institution").asScala.toList.map(_.trim().toUpperCase())
                val storepng = getOptionalString("store-png-path")
                val encoding = c.getString("encoding")
                val processBegin = getOptionalString("process-begin").map(LocalDate.parse)
                val processDayBehind = getOptionalInt("process-day-behind").getOrElse(1)
                val pauseInterval = getOptionalInt("pause-interval").getOrElse(0)
                val calev = getOptionalString("calendar-event")
                val webport = getOptionalInt("web-port-number")
                val drlEditIps = c.getStringList("drl-edit-ips").asScala.toList
                val db: DB = 
                    { for
                        db   <- getOptionalString("postgres-db")
                        user <- getOptionalString("postgres-user")
                        pass <- getOptionalString("postgres-password")
                    yield Postgresql(db, user, pass) }.getOrElse(SQLite())

                assert(processDayBehind >= 0 && pauseInterval >= 0)
                // TODO : add printIp option as command line argument
                CTDoseConfig(ci, db, tpath, isDLP, institutionNames, storepng, encoding, 
                    processBegin, processDayBehind, pauseInterval, calev, webport, drlEditIps, true)
            .toEither.left.map(_.getMessage())
        else Left(s"Cannot find $fname.conf")

    def loadConfigAndRun[A](f: (CTDoseConfig, CTInfo) => IO[A]) = 
        val c = Configuration().flatMap(c => ctInfo().map((c, _)))
        for 
            r <- c match
                    case Left(value) => 
                        IO(println(value))
                    case Right((cdc, m)) =>
                        Tesseract.setTesseractPath(cdc.tpath)
                        f(cdc, m)
        yield r 
package net.maryknollrad.httpserver

import cats.effect.*
import org.http4s.* 
import org.http4s.dsl.io.*
import net.maryknollrad.ctdose.DB
import net.maryknollrad.ctdose.Configuration.CTDoseConfig
import net.maryknollrad.ctdose.DB.{QueryInterval, QueryPartition}
import doobie.*, doobie.implicits.* 
import cats.syntax.all.* 
import upickle.default.{ReadWriter => RW, macroRW, write}
import fs2.{Stream, Pipe}

object Api: 
    def optionInterval(argument: String): Option[QueryInterval] =
        QueryInterval.values.find(_.strValue == argument)    
    
    def optionPartition(argument: String): Option[QueryPartition] =
        QueryPartition.values.find(_.strValue == argument)

    def pAndI[A](partition: String, interval: String)(f:(QueryPartition, QueryInterval) => A): Option[A] = 
        for 
            p <- optionPartition(partition)
            i <- optionInterval(interval)
        yield f(p, i)

case class Api(config: CTDoseConfig):
    import Api.*
    import scala.util.{Try, Success, Failure}
    import fs2.data.csv.*
    import fs2.data.csv.generic.semiauto
    import org.http4s.headers.*
    
    given encoder: CsvRowEncoder[DB.DrlResult, String] = semiauto.deriveCsvRowEncoder
    given drlEncoder: EntityEncoder[IO, Stream[IO, DB.DrlResult]] =
        org.http4s.fs2data.csv.csvEncoderForPipe(encodeUsingFirstHeaders(fullRows = true))

    private def bpartsCounts(qi: QueryInterval, from: Int, to: Int) = 
        config.db.getBodypartCounts(qi, from, to).map(ts =>
            val (cats, counts) = ts.foldLeft((Seq.empty[String], Seq.empty[Long]))({ 
                case ((cats, counts), t) => (cats :+ t._1, counts :+ t._2)
            })
            (cats, counts)
        )

    private def doseBox(qi: QueryInterval, from: Int, to: Int) = 
        config.db.partitionedQuery(QueryPartition.Bodypart, qi, from = from, to = to)
            .map(rs => Data.toBoxedMap(rs))

    private val queryStrings = Seq("day", "week", "month", "year")

    val apiService = HttpRoutes.of[IO] {
        case GET -> Root / "echo" / words =>
            println(words)
            Ok(words)

        case GET -> Root / "graphdata" / IntVar(i) if i >= 0 && i <= QueryInterval.qiSize =>
            case class GraphData(bodyparts: Seq[String], bodypartsCounts: Seq[Long], 
                bodypartBox: Map[String, Map[String, Seq[Double]]]) derives RW
            val qi = QueryInterval.fromOrdinal(i)
            // if selected interval is day, show yesterday else this interval
            val (from, to) = QueryInterval.defaultRange(qi)
            bpartsCounts(qi, from, to).flatMap(bparts =>
                doseBox(qi, from, to).flatMap(boxmap => 
                    Ok(write(GraphData(bparts._1, bparts._2, boxmap)))
                ))

        case GET -> Root / "trends" / partition / partitionValue / interval =>
            pAndI(partition, interval)((pt, it) =>
                val (from, to) = it match
                    case QueryInterval.Day => (7, 0)
                    case QueryInterval.Week => (10, 0)
                    case QueryInterval.Month => (12, 0)
                    case QueryInterval.Year => (5, 0)
                config.db.partitionedQuery(pt, it, Some(partitionValue), from, to).flatMap(ps =>
                    Ok(write(Data.toBoxedMap(ps, _.dateNumber)))
                )).getOrElse(NotFound())

        case req @ GET -> Root / "setdrl" / cidStr / sidStr / dlabel if config.drlEditable(req) =>
            cidStr.toIntOption.flatMap(cid => sidStr.toIntOption.map(sid => (cid, sid))) match 
                case Some((cid, sid)) =>
                    config.db.updateDrl(cid.toInt, sid.toInt, dlabel).flatMap(_ match
                        case 1 => 
                            Ok("success")
                        case n =>
                            Ok(s"Number of updated study is not one : $n")
                    )
                case _ =>
                    Ok("Failed to convert cid/sid string value")

        case GET -> Root/"drlcsv"/categoryName/IntVar(intervalIndex) =>                
            import org.typelevel.ci.CIString
            import java.time.LocalDate
            import java.time.format.DateTimeFormatter.BASIC_ISO_DATE
            import QueryInterval.*

            val qi = QueryInterval.fromOrdinal(intervalIndex)
            val fname = s"drlresult_${categoryName}_${queryStrings(intervalIndex)}_${LocalDate.now().format(BASIC_ISO_DATE)}.csv"
            val (from, to) = qi match 
                case Day => (1, 1)
                case _ => (0, 0)
            config.db.drlFull(categoryName, qi, from, to, config.doseDLP).flatMap(rs =>
                Ok(Stream.emits(rs).asInstanceOf[Stream[IO, DB.DrlResult]], 
                    `Content-Disposition`("attachment", Map(CIString("filename") -> fname)))
            )
    }
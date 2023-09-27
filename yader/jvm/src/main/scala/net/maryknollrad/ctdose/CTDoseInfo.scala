package net.maryknollrad.ctdose

import org.dcm4che3.data.{Tag, ElementDictionary}
import net.maryknollrad.d4cs.DicomBase.* 
import net.maryknollrad.ctdose.CTDose.DoseResultRaw
import java.time.{LocalDate, LocalTime}
import java.time.format.DateTimeFormatter
import doobie.implicits.*
import java.lang.System
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import cats.effect.IO
import cats.syntax.all.* 
import scala.util.Try
import DB.{Study, Patient}
import net.maryknollrad.ctdose.SQLite.*
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*
import org.slf4j.{Logger, LoggerFactory}
import com.github.eikek.calev.CalEvent
import eu.timepit.fs2cron.calev.CalevScheduler
import fs2.Stream

object CTDoseInfo:
    type CTInfo = Map[(String, String), CTDoseSeriesInfo]

    val DoseBy = "dose-series-by"
    val DoseValue = "dose-series-value"
    val ExtReg = "dose-extraction-reg"
    private val logger = LoggerFactory.getLogger(getClass)

    enum CTDoseSeries:
        case SeriesNumber, SeriesIndex, SeriesName

    type CTDoseInfoValue = Int | String

    case class DoseSeriesInfo(by: Option[CTDoseSeries] = None, value: Option[CTDoseInfoValue] = None, regex: Option[String] = None):
        def allNonEmpty() = by.nonEmpty && value.nonEmpty && regex.nonEmpty
        def missingField() = 
            Seq((by, DoseBy), (value, DoseValue), (regex, ExtReg)).foldLeft(Seq.empty[String]):
                case (ans, (f, fs)) => 
                    if f.isEmpty then ans :+ fs
                    else ans
    
    case class CTDoseSeriesInfo(manufacturer: String, model: String, seriesInfo: CTDoseInfo.DoseSeriesInfo)
    case class CTDoseResult(studyInfo: Seq[(Int, String)], results: Seq[DoseResultRaw], emptyAttrs: Seq[Int])
    
    private val timeformatter = DateTimeFormatter.ofPattern("HHmmss.SSS")
    def makeInsertables(cdr: CTDoseResult, ctags: Seq[Int], storeflag: Option[String]): Either[String, (Study, Patient, IO[Option[Boolean]], IO[Option[Int]])] = 
        val imap = cdr.studyInfo.toMap
        val doseResult = cdr.results.find(_.ocrResult.nonEmpty)
        val dose = doseResult.flatMap(_.ocrResult).getOrElse(-1.0)
        for 
            study       <- Try {
                            Study(imap(Tag.AccessionNumber), imap(Tag.PatientID), 
                                LocalDate.parse(imap(Tag.StudyDate), DateTimeFormatter.BASIC_ISO_DATE), 
                                LocalTime.parse(imap(Tag.StudyTime), timeformatter),
                                imap(Tag.StudyDescription), imap(Tag.ProtocolName), imap(Tag.BodyPartExamined), 
                                imap(Tag.Manufacturer), imap(Tag.ManufacturerModelName), imap(Tag.StationName), 
                                imap(Tag.OperatorsName), imap(Tag.ReferringPhysicianName), 0.0, 0.0) 
                            }.toEither.left.map(t => s"Error occurred during preparing insertion of study : ${t.getMessage()}")
            patient     <- Try(
                            DB.Patient(imap(Tag.PatientID), imap(Tag.PatientSex).trim, 
                                LocalDate.parse(imap(Tag.PatientBirthDate), DateTimeFormatter.BASIC_ISO_DATE)
                            )).toEither.left.map(t => s"Error occurred during preparing insertion of patient : ${t.getMessage()}")
            imgIO       =  storeflag.flatMap(p =>
                            doseResult.map(dr => IO.blocking {
                                val folder = Seq(p, imap(Tag.StudyDate)).foldLeft(os.pwd)(_ / _)
                                os.makeDir.all(folder)
                                val fname = folder / s"${study.accessionNumber}.png"
                                val of = fname.toIO
                                javax.imageio.ImageIO.write(dr.image, "png", of)
                            }))
            warnEmpty   =   Option.when(cdr.emptyAttrs.nonEmpty) :
                                val emptyTagNames = cdr.emptyAttrs.map(tag => ElementDictionary.keywordOf(tag, null)).mkString(",")
                                val tagStr = if cdr.emptyAttrs.length == 1 then "tag" else "tags"
                                val msg = s"Accession number : ${imap(Tag.AccessionNumber)} has following empty dicom $tagStr - $emptyTagNames"
                                SQLite.log(msg, DB.LogType.Warn)
        yield (study, patient, imgIO.sequence, warnEmpty.sequence)

    def getDoseReportAndStore(conf: Configuration.CTDoseConfig, ctInfo: CTInfo, d: LocalDate = LocalDate.now(), collectTags: Seq[Int]) = 
        for
            startTime           <-  IO.blocking(System.currentTimeMillis()) // prevents parallel execution 
            successesAndFails   <-  CTDose.getCTDoses(conf, ctInfo, d, collectTags)
            (successes, fails)  =   successesAndFails
            _                   <-  SQLite.logs(fails, DB.LogType.Error)
            stored              <-  successes.traverse(cdr =>
                                        makeInsertables(cdr, CTDose.getDefaultCollectTags(), conf.storepng)
                                            .traverse({
                                                case (study, patient, oImageIo, oEmptyAttrWarnIo) =>
                                                    SQLite.insertStudyAndPatient(study, patient)
                                                    *> oImageIo
                                                    *> oEmptyAttrWarnIo
                                            }))
            (stores, storef)    =   stored.partition(_.isRight).bimap(_.flatMap(_.toOption), _.flatMap(_.swap.toOption))
            _                   <-  SQLite.logs(storef, DB.LogType.Error)
            _                   <-  if successes.length == stores.length then {
                                        val dStr = d.format(DateTimeFormatter.ISO_LOCAL_DATE)
                                        val runtime = String.format("%.1f", (System.currentTimeMillis() - startTime) / 1000.0)
                                        val msg = s"$dStr : processed ${successes.length} studies in ${runtime}s"
                                        SQLite.log(msg, DB.LogType.Info)
                                    } else
                                        IO.pure(0)
        yield ()

    extension[A] (as: Seq[A])
        def intervene(i: A): Seq[A] = 
            def h(aas: Seq[A], acc: Seq[A]): Seq[A] = 
                if aas.length <= 1 then acc ++ aas
                else h(aas.tail, acc ++ Seq(aas.head, i))
            h(as, Seq.empty)

    def processDoseReport(conf: Configuration.CTDoseConfig, ctInfo: CTInfo, collectTags: Option[Seq[Int]] = None) = 
        val ctags = collectTags.getOrElse(CTDose.getDefaultCollectTags())
        for 
            _               <-  SQLite.createTablesIfNotExists()
            olastDate       <-  SQLite.getLastDateProcessed()
            _               <-  { 
                                val beginDate = olastDate match
                                    case Some(d) => d.plusDays(1)
                                    case _ => conf.processBegin.getOrElse(LocalDate.now().minusDays(conf.processDayBehind))
                                val endDate = LocalDate.now().minusDays(conf.processDayBehind)
                                val dstream = beginDate.datesUntil(endDate.plusDays(1)).iterator().asScala.toSeq
                                val lmsg = if !beginDate.isAfter(endDate) then s"Processing $beginDate ~ $endDate"
                                           else s"Skipping $beginDate"
                                SQLite.log(lmsg, DB.LogType.Debug)
                                *> 
                                dstream.map(d =>
                                    logger.info("Processing date : {}", d)
                                    getDoseReportAndStore(conf, ctInfo, d, ctags) 
                                    *> SQLite.updateLastDateProcessed(d) *> IO.unit
                                ).intervene(IO.sleep(conf.pauseInterval.seconds)).sequence
                                }
        yield ()

    import Configuration.CTDoseConfig
    def run(cdi: CTDoseConfig, cti: CTInfo) = 
        val calevScheduler = CalevScheduler.systemDefault[IO]
        val jobio = CTDoseInfo.processDoseReport(cdi, cti)
        cdi.calendarEvent match 
            case Some(schedule) =>
                val runtime = CalEvent.unsafe(schedule)
                val scheduled = calevScheduler.awakeEvery(runtime) >> Stream.eval(jobio)
                logger.info("RUNNING IN SERVER MODE.")
                jobio >> scheduled.compile.drain
            case None => 
                logger.info("RUNNING PLAIN MODE")
                jobio

package net.maryknollrad.ctdose

import org.dcm4che3.data.Tag
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
import net.maryknollrad.ctdose.SQLite.log

object CTDoseInfo:
    type CTInfo = Map[(String, String), CTDoseSeriesInfo]

    val DoseBy = "dose-series-by"
    val DoseValue = "dose-series-value"
    val ExtReg = "dose-extraction-reg"

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
    case class CTDoseResult(studyInfo: Seq[(Int, String)], results: Seq[DoseResultRaw])

    def makeInsertables(cdr: CTDoseResult, ctags: Seq[Int], storeflag: Option[String]): Either[String, (Study, Patient, Option[IO[Boolean]])] = 
        val imap = cdr.studyInfo.toMap
        val doseResult = cdr.results.find(_.ocrResult.nonEmpty)
        val dose = doseResult.flatMap(_.ocrResult).getOrElse(-1.0)
        for 
            study   <- Try {
                        Study(imap(Tag.AccessionNumber), imap(Tag.PatientID), 
                            LocalDate.parse(imap(Tag.StudyDate), DateTimeFormatter.ISO_LOCAL_DATE), 
                            LocalTime.parse(imap(Tag.StudyTime), DateTimeFormatter.ISO_LOCAL_TIME),
                            imap(Tag.StudyDescription), imap(Tag.ProtocolName), imap(Tag.BodyPartExamined), 
                            imap(Tag.Manufacturer), imap(Tag.ManufacturerModelName), imap(Tag.StationName), 
                            imap(Tag.OperatorsName), dose) 
                        }.toEither.left.map(t => s"Error occurred during preparing study : ${t.getMessage()}")
            patient <- Try(
                        DB.Patient(imap(Tag.PatientID), imap(Tag.PatientSex).trim.head, 
                            LocalDate.parse(imap(Tag.PatientBirthDate), DateTimeFormatter.ISO_LOCAL_DATE)
                        )).toEither.left.map(t => s"Error occurred during preparing patient : ${t.getMessage()}")
            imgIO   =  storeflag.flatMap(p =>
                            doseResult.map(dr => IO {
                                val fname = java.nio.file.Paths.get(".", p, study.accessionNumber)
                                val of = fname.toFile()
                                javax.imageio.ImageIO.write(dr.image, "png", of)
                            }))
        yield (study, patient, imgIO)

    private def logMsg(d: LocalDate, successes: Seq[CTDoseResult], fails: Seq[String], store: Seq[Either[String, _]], startTime: Long) = 
        val dStr = d.format(DateTimeFormatter.BASIC_ISO_DATE)
        val (storeSuccesses, storeFails) = store.partition(_.isRight).bimap(_.flatMap(_.toOption), _.flatMap(_.swap.toOption))
        s"$dStr : ${successes.length} successful and ${fails.length} failed DICOM operations. " ++
            s"${storeSuccesses.length} successful ${storeFails.length} DB operations in ${System.currentTimeMillis() - startTime}ms. " ++
            s"${(fails ++ storeFails).mkString("Failure messages : [", ",", "]")}"

    def getDoseReportAndStore(conf: Configuration.CTDoseConfig, ctInfo: CTInfo, d: LocalDate = LocalDate.now(), collectTags: Option[Seq[Int]] = None) = 
        val startTime = System.currentTimeMillis
        val ctags = collectTags.getOrElse(CTDose.getDefaultCollectTags())
        for
            _                   <-  SQLite.createTablesIfNotExists(ctags)
            successesAndFails   <-  CTDose.getCTDoses(conf, ctInfo, d, ctags)
            (successes, fails)  =   successesAndFails
            stored              <-  successes.traverse(cdr =>
                                        makeInsertables(cdr, CTDose.getDefaultCollectTags(), conf.storepng)
                                            .traverse({
                                                case (study, patient, oIo) =>
                                                    SQLite.insertStudyAndPatient(study, patient)
                                                    *> oIo.traverse(io => io)
                                            }))
            _                   <-  SQLite.log(logMsg(d, successes, fails, stored, startTime)).transact(SQLite.xa)
        yield ()

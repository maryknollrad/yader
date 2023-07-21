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

    def makeInsertables(cdr: CTDoseResult, ctags: Seq[Int], storeflag: Boolean) = 
        val imap = cdr.studyInfo.toMap
        val doseResult = cdr.results.find(_.ocrResult.nonEmpty)
        val dose = doseResult.flatMap(_.ocrResult).getOrElse(-1.0)
        val img = 
            if storeflag then doseResult.map(dr =>
                val os = ByteArrayOutputStream()
                ImageIO.write(dr.image, "png", os) 
                Some(os.toByteArray())
            ) else 
                None
        val study0 = (imap(Tag.AccessionNumber), imap(Tag.PatientID))
        val study1 = Tuple.fromArray(ctags.drop(4).map(imap).toArray)
        val study2 = (dose, img)

        val patient = (imap(Tag.PatientID), imap(Tag.PatientSex), imap(Tag.PatientBirthDate))

        (study0 ++ study1 ++ study2, patient)

    /*
    type DBField = String | LocalDate | LocalTime | Double | Option[Array[Byte]]
    type A = String *: String *: (scala.Tuple match {
            case scala.EmptyTuple => (scala.Double, Option[Some[Array[Byte]]])
            case x1 *: xs1 => x1 *: scala.Tuple.Concat[xs1, (scala.Double, Option[Some[Array[Byte]]])]
        } <: scala.Tuple
    */
    def getDoseReportAndStore(conf: Configuration.CTDoseConfig, ctInfo: CTInfo, d: LocalDate = LocalDate.now(), collectTags: Option[Seq[Int]] = None) = 
        val startTime = System.currentTimeMillis
        val ctags = collectTags.getOrElse(CTDose.getDefaultCollectTags())
        for
            _                   <-  SQLite.createTablesIfNotExists(ctags)
            successesAndFails   <-  CTDose.getCTDoses(conf, ctInfo, d, ctags)
            (successes, fails)  = successesAndFails
            // midtime             = System.currentTimeMillis()
            // _                   <-  IO.println(s"${d.format(DateTimeFormatter.BASIC_ISO_DATE)} : ${successes.length} successful and ${fails.length} failed DICOM operations in ${System.currentTimeMillis() - startTime}ms. ${fails.mkString("Failure messages : [", ",", "]")}")
            _                   <-  successes.map(makeInsertables(_, CTDose.getDefaultCollectTags(), conf.storepng))
                                        .traverse(t => 
                                            SQLite.insertStudyAndPatient(t._1, t._2))
            _                   <-  SQLite.log(s"${d.format(DateTimeFormatter.BASIC_ISO_DATE)} : ${successes.length} successful and ${fails.length} failed DICOM operations in ${System.currentTimeMillis() - startTime}ms. ${fails.mkString("Failure messages : [", ",", "]")}").transact(SQLite.xa)
        yield ()

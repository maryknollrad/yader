import net.maryknollrad.d4cs.{CFind, CGet, DicomBase, RetrieveLevel, DicomTags}
import net.maryknollrad.ctdose.{Configuration, Tesseract, CTDose, CTDoseInfo, CTDRL, DB}
import net.maryknollrad.ctdose.SQLite

import DicomBase.*
import java.time.{LocalDate, LocalTime, LocalDateTime}
import java.time.format.DateTimeFormatter
import org.dcm4che3.data.Tag
import RetrieveLevel.*
import DicomBase.*
import cats.effect.*
import org.dcm4che3.data.Attributes
import Configuration.ConnectionInfo
import java.awt.image.BufferedImage
import CTDose.DoseResultRaw
import CTDoseInfo.*
import org.dcm4che3.data.ElementDictionary
import net.maryknollrad.httpserver.HttpServer
import cats.syntax.all.*

case class CT(chartNo: String, patientName: String, patientSex: String, patientAge: Int, 
                studyDateTime: LocalDateTime, description: String, studyID: String)

object Demo extends IOApp:
    import DicomBase.{String2LocalDate, String2LocalTime, LocalDate2String}

    def run(as: List[String]): IO[ExitCode] = 
        // loadConfigAndRun(printTodaysExams()) *> IO(ExitCode.Success)
        // loadConfigAndRun(cgetTest2(iiuid)).flatMap({
        //     case im: BufferedImage => IO(savepng(s"$iiuid.png", im))
        // })  *> IO(ExitCode.Success)
        // loadConfigAndRun(cgetTest2(iiuid)).flatMap({
        //     case im: BufferedImage => IO(ocr(im))
        // })  *> IO(ExitCode.Success)
        // "trace", "debug", "info", "warn", "error" or "off"
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "error")
        // Configuration.loadConfigAndRun(dose)
        // ct_info()
        // SQLite.createTablesIfNotExists()
        // tagTest()
        // Configuration.loadConfigAndRun({
            // case (cdi, cti) => CTDoseInfo.getDoseReportAndStore(cdi, cti, LocalDate.now().minusDays(1L), CTDose.getDefaultCollectTags())})
            // case (cdi, cti) => CTDoseInfo.processDoseReport(cdi, cti)})
            // case (cdi, cti) => dose(cdi, cti)})
        // (CTDoseInfo.run(), HttpServer.server).parMapN { (_, _) => () }
        HttpServer.server 
        // CTDoseInfo.run()
        // IO.println(CTDRL())
        // dbTest()
        *> IO(ExitCode.Success)

    def dbTest() = 
        import DB.QueryPartition.* 
        import DB.QueryInterval.*
        import doobie.*, doobie.implicits.*

        SQLite.partitionedQuery(Bodypart, Week, None, 3)
            .flatMap(rs => IO.println(rs))

    def tagTest() = 
        // IO.println(CTDose.getDefaultCollectTags().map(t =>
        //     s"${ElementDictionary.keywordOf(t, null)} : ${ElementDictionary.vrOf(t, null)}").mkString(","))
        IO.println(SQLite.createStudyTableSQL(CTDose.getDefaultCollectTags()).toString)

    def ct_info() = 
        IO.println(Configuration.ctInfo())

    def ocr(image: BufferedImage) =
        val r = Tesseract.doOCR(image)
        println(r)

    // private val aquillionPrime = """Total DLP.+\(Head\):\s*([-.0-9]+)\s*\(Body\):\s*([-.0-9]+)""".r.unanchored

    import scala.util.matching.Regex
    def printIfFound(s: String, r: Regex) = 
        r.findFirstMatchIn(s) match
            case Some(m) => println(s"""$m => ${CTDose.findFirstDoubleStringInMatch(m)}""")
            case None => println("FAILED TO FIND DATA")

    def mapSummary(map: Map[String, String]) = 
        map.keys.foreach: k =>
            if k.startsWith("STUDY") then println(s"${k.drop(6)} : ${map(k)}")
            if k.startsWith("I") then println(s"$k : ${map(k)}")

    def dose(c: Configuration.CTDoseConfig, m: CTInfo) =
        for 
            t2:(Seq[CTDoseResult], Seq[String]) <-CTDose.getCTDoses(c, m, d = LocalDate.now())
            _ <- IO:
                val (successes, fails) = t2
                println(s"Got ${successes.length} successful images and ${fails.length} Failures.")
                fails.zipWithIndex.foreach(println)
                successes.zipWithIndex.foreach: 
                    case (cdr, i) => 
                        println(cdr.studyInfo.toMap)
                        println("-"*80)
                        cdr.results.zipWithIndex.foreach: 
                            (doseResult, j) =>
                                // savepng(s"$i-$j.png", doseResult.image)
                                // println(s"${doseResult.acno} ($i-$j) : ${printIfFound(doseResult.ocrRaw, aquillionPrime)}")
                                println(s"${doseResult.acno} ($i-$j) : ${doseResult.ocrResult.getOrElse("FAILED TO FIND DATA")}")
        yield ()

    def printTodaysExams(modality: String = "CT")(ci: ConnectionInfo) = 
        val dtag = DicomTags((Tag.ModalitiesInStudy, "CT"), (Tag.StudyDate, LocalDate.now():String),
               Tag.PatientID, Tag.PatientName, Tag.PatientSex, Tag.PatientAge, Tag.StudyTime, Tag.StudyDescription, Tag.StudyInstanceUID)

        val r = CFind(ci.callingAe, ci.calledAe, ci.host, ci.port, ci.encoding)

        def retriveCTInfo(tags: Seq[StringTag]): CT = 
            val chartno = tags.find(_.tag == Tag.PatientID).map(_.value).get
            val name = tags.find(_.tag == Tag.PatientName).map(_.value).get
            val sex = tags.find(_.tag == Tag.PatientSex).map(_.value).get
            val age = tags.find(_.tag == Tag.PatientAge).map(_.value.init.toInt).get
            val sdate: LocalDate = tags.find(_.tag == Tag.StudyDate).map(_.value).get
            val stime: LocalTime = tags.find(_.tag == Tag.StudyTime).map(_.value).get
            val description = tags.find(_.tag == Tag.StudyDescription).map(_.value).get
            val studyuid = tags.find(_.tag == Tag.StudyInstanceUID).map(_.value).get
            CT(chartno, name, sex, age, LocalDateTime.of(sdate, stime), description, studyuid)

        for
            _ <- IO:
                    println(s"Today's $modality exams")
                    println("-" * 80)
            cts <- r.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, 
                    Some(StudyLevel), dtag)
            _   <- IO:
                    cts.map(retriveCTInfo).foreach(println)
                    println(s"Got ${cts.length} results")
        yield ()

    private val iiuid = "1.2.410.200010.20220318.114440.100041.132583.78541"
    // store to file
    def cgetTest1(imageInstanceUID: String = iiuid)(ci: ConnectionInfo) =  
        val r = CGet(ci.callingAe, ci.calledAe, ci.host, ci.port)
        val dtag = DicomTags((Tag.SOPInstanceUID, imageInstanceUID))
        for 
            _ <- IO: 
                    println("Getting image from PACS server")
            _ <- r.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), dtag)
        yield ()

    private def savepng(fname: String, image: BufferedImage) = 
        val of = java.io.File(fname)
        javax.imageio.ImageIO.write(image, "png", of)

    // store to buffer, and then convert to png file
    def cgetTest2(imageInstanceUID: String = iiuid)(ci: ConnectionInfo): IO[BufferedImage] =  
        import org.dcm4che3.imageio.plugins.dcm.*   // register "DICOM" ImageReader
        import org.dcm4che3.io.DicomInputStream
        import javax.imageio.ImageIO

        val r = CGet(ci.callingAe, ci.calledAe, ci.host, ci.port, false)
        val dtag = DicomTags((Tag.SOPInstanceUID, imageInstanceUID))
        val reader = ImageIO.getImageReadersByFormatName("DICOM").next()
        for 
            _ <- IO: 
                    println("Getting image from PACS server")
            _ <- r.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), dtag)
            image <- IO:
                    val dis = r.getDicomInputStreamAndFree(imageInstanceUID)
                    reader.setInput(dis)
                    reader.read(0, reader.getDefaultReadParam())
        yield image
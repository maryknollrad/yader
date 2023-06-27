import net.maryknollrad.d4cs.{CFind, CGet, DicomBase, RetrieveLevel, DicomTags}
import DicomBase.*
import java.time.{LocalDate, LocalTime, LocalDateTime}
import java.time.format.DateTimeFormatter
import org.dcm4che3.data.Tag
import RetrieveLevel.*
import DicomBase.*
import cats.effect.*
import org.dcm4che3.data.Attributes

case class CT(chartNo: String, patientName: String, patientSex: String, patientAge: Int, 
                studyDateTime: LocalDateTime, description: String, studyID: String)

object Demo extends IOApp:
    import DicomBase.{String2LocalDate, String2LocalTime, LocalDate2String}

    def run(as: List[String]): IO[ExitCode] = 
        // printTodaysExams() *> IO(ExitCode.Success)
        cgetTest2(iiuid) *> IO(ExitCode.Success)

    def printTodaysExams(modality: String = "CT") = 
        val dtag = DicomTags((Tag.ModalitiesInStudy, "CT"), (Tag.StudyDate, LocalDate.now():String),
               Tag.PatientID, Tag.PatientName, Tag.PatientSex, Tag.PatientAge, Tag.StudyTime, Tag.StudyDescription, Tag.StudyInstanceUID)

        val r = CFind("READROOM", "NETGEAR_EXTERNAL", "192.168.10.133", 105, "euc_kr")

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
    def cgetTest1(imageInstanceUID: String) =  
        val r = CGet("READROOM", "NETGEAR_EXTERNAL", "192.168.10.133", 105, true)
        val dtag = DicomTags((Tag.SOPInstanceUID, imageInstanceUID))
        for 
            _ <- IO: 
                    println("Getting image from PACS server")
            _ <- r.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), dtag)
        yield ()

    // store to buffer, and then convert to png file
    def cgetTest2(imageInstanceUID: String) =  
        import org.dcm4che3.imageio.plugins.dcm.*   // register "DICOM" ImageReader
        import org.dcm4che3.io.DicomInputStream
        import javax.imageio.ImageIO

        val r = CGet("READROOM", "NETGEAR_EXTERNAL", "192.168.10.133", 105, false)
        val dtag = DicomTags((Tag.SOPInstanceUID, imageInstanceUID))
        val reader = ImageIO.getImageReadersByFormatName("DICOM").next()
        for 
            _ <- IO: 
                    println("Getting image from PACS server")
            _ <- r.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), dtag)
            _ <- IO:
                    val is = r.getImageStorage()
                    val dis = DicomInputStream(java.io.ByteArrayInputStream(is(imageInstanceUID).toByteArray()))
                    reader.setInput(dis)
                    val i = reader.read(0, reader.getDefaultReadParam())
                    val of = java.io.File(s"${imageInstanceUID}.png")
                    javax.imageio.ImageIO.write(i, "png", of)
        yield ()
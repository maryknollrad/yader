import net.maryknollrad.d4cs.{CFind, DicomBase, RetrieveLevel, DicomTags}
import DicomBase.*
import java.time.{LocalDate, LocalDateTime}
import org.dcm4che3.data.Tag
import RetrieveLevel.*
import DicomBase.{LocalDate2String, String2LocalDate, String2LocalTime}
import cats.effect.*
import org.dcm4che3.data.Attributes

// case class CT(chartNo: String, patientName: String, patientSex: String, patientAge: Int, 
//                 studyDateTime: LocalDateTime, description: String)

case class CT(chartNo: String, patientName: String, description: String)
object TodaysCT extends IOApp:
    def run(as: List[String]): IO[ExitCode] = 
        printTodaysExams() *> IO(ExitCode.Success)

    def printTodaysExams(modality: String = "CT") = 
        val dtag = DicomTags((Tag.ModalitiesInStudy, "CT"), (Tag.StudyDate, LocalDate.now():String),
               Tag.PatientID, Tag.PatientName, Tag.PatientSex, Tag.PatientAge, Tag.StudyTime, Tag.StudyDescription, Tag.StudyInstanceUID)

        val r = new CFind("READROOM", "NETGEAR_EXTERNAL", "192.168.10.133", 105) {}

        def retriveCTInfo(dtags: DicomTags, attr: Attributes): CT = 
            import DicomTags.getStringTag
            // val sex = getStringTag(Tag.PatientSex, attr)
            // val age = getStringTag(Tag.PatientSex, attr).init.toInt
            // val sdate = getStringTag(Tag.StudyDate, attr)
            // val stime = getStringTag(Tag.StudyTime, attr)
            // val sdatetime = LocalDateTime.of(sdate, stime)
            CT(getStringTag(Tag.PatientID, attr, "euc_kr"), getStringTag(Tag.PatientName, attr, "euc_kr"), // sex, age,
                getStringTag(Tag.StudyDescription, attr, "euc_kr"))
            // CT(getStringTag(Tag.PatientID, attr, "euc_kr"), getStringTag(Tag.PatientName, attr, "euc_kr"), sex, age,
            //     sdatetime, getStringTag(Tag.StudyDescription, attr, "euc_kr"))

        for
            _ <- IO:
                    println(s"Today's $modality exams")
                    println("-" * 80)
            cts <- r.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, 
                    Some(StudyLevel), dtag, retriveCTInfo)
            _   <- IO:
                    cts.foreach(println)
                    println(s"Got ${cts.length} results")
        yield ()

import net.maryknollrad.d4cs.{CFind, DicomBase, RetrieveLevel, DicomTags}
import DicomBase.*
import java.time.{LocalDate, LocalDateTime}
import org.dcm4che3.data.Tag
import RetrieveLevel.*
import DicomBase.{Date2String, String2DateTime}
import cats.effect.*
import org.dcm4che3.data.Attributes

case class CT(chartNo: String, patientName: String, patientSex: String, patientAge: Int, 
                studytime: LocalDateTime, description: String)

object TodaysCT extends IOApp:
    def run(as: List[String]): IO[ExitCode] = 
        printTodaysExams() *> IO(ExitCode.Success)

    def printTodaysExams(modality: String = "CT") = 
        val dtag_old: DicomTags = Seq[DicomTag]((Tag.ModalitiesInStudy, "CT"), (Tag.StudyDate, LocalDate.now():String),
            Tag.PatientID, Tag.PatientName, Tag.StudyDescription, Tag.StudyTime, Tag.StudyInstanceUID)
        val dtag = DicomTags((Tag.ModalitiesInStudy, modality), (Tag.StudyDate, LocalDate.now():String),
                        Tag.PatientID, Tag.PatientName, Tag.PatientSex, Tag.PatientAge, 
                        Tag.StudyDate, Tag.StudyTime, Tag.StudyDescription, Tag.StudyInstanceUID)

        val r = new CFind("READROOM", "NETGEAR_EXTERNAL", "192.168.10.133", 105) 

        def retriveCTInfo(dtags: DicomTags, attr: Attributes): CT = 
            import DicomTags.getStringTag
            val sex = getStringTag(Tag.PatientSex, attr)
            val age = attr.getInt(Tag.PatientAge, 0)
            val sdate = String(attr.getBytes(Tag.StudyDate))
            val stime = String(attr.getBytes(Tag.StudyTime))
            val sdt = sdate ++ stime
            // val sdt = sdate ++ " " ++ stime
            CT(getStringTag(Tag.PatientID, attr, "euc_kr"), getStringTag(Tag.PatientName, attr, "euc_kr"), sex, age, 
                sdt, getStringTag(Tag.StudyDescription, attr, "euc_kr"))

        for
            _ <- IO:
                    println(s"Today's $modality exams")
                    println("-" * 80)
            cts <- r.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, 
                    Some(StudyLevel), dtag_old, retriveCTInfo)
            _   <- IO:
                    cts.foreach(println)
                    println(s"Got ${cts.length} exams")
                    println(dtag_old == dtag)
        yield ()
import net.maryknollrad.d4cs.{CFind, DicomTags, DicomBase, RetrieveLevel}
import java.time.{LocalDate, LocalDateTime}
import org.dcm4che3.data.Tag
import RetrieveLevel.*
import DicomBase.LocalDate2String
import cats.effect.*
import org.dcm4che3.data.Attributes

case class Runner(callingAe: String, calledAe: String, remoteHost: String, remotePort: Int) extends CFind(callingAe, calledAe, remoteHost, remotePort)

case class CT(chartNo: String, patientName: String, /* studytime: LocalDateTime, */ description: String)

object TodaysCT extends IOApp:
    def run(as: List[String]): IO[ExitCode] = 
        val dtag = DicomTags(Map(Tag.ModalitiesInStudy -> "CT", Tag.StudyDate -> LocalDate.now()),
            Seq(Tag.PatientID, Tag.PatientName, Tag.StudyDescription, Tag.StudyTime, Tag.StudyInstanceUID))

        val r = Runner("READROOM", "NETGEAR_EXTERNAL", "192.168.10.133", 105)

        def retriveCTInfo(dtags: DicomTags, attr: Attributes): CT = 
            import DicomBase.getTag
            CT(getTag(Tag.PatientID, attr, "euc_kr"), getTag(Tag.PatientName, attr, "euc_kr"), getTag(Tag.StudyDescription, attr, "euc_kr"))

        for
            _ <- IO:
                    println("Today's CT exams")
                    println("-" * 80)
            cts <- r.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, 
                    Some(StudyLevel), dtag, retriveCTInfo)
            _   <- IO:
                    cts.foreach(println)
        yield ExitCode.Success

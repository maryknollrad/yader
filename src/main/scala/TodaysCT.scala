import net.maryknollrad.d4cs.{CFind, DicomTags, DicomBase, RetrieveLevel}
import java.time.LocalDate
import org.dcm4che3.data.Tag
import RetrieveLevel.*
import DicomBase.LocalDate2String
import cats.effect.*

case class Runner(callingAe: String, calledAe: String, remoteHost: String, remotePort: Int) extends CFind(callingAe, calledAe, remoteHost, remotePort)

object TodaysCT extends IOApp:
    def run(as: List[String]): IO[ExitCode] = 
        val dtag = DicomTags(Map(Tag.ModalitiesInStudy -> "CT", Tag.StudyDate -> LocalDate.now()),
            Seq(Tag.PatientID, Tag.PatientName, Tag.StudyDescription, Tag.StudyTime, Tag.StudyInstanceUID))

        val r = Runner("READROOM", "NETGEAR_EXTERNAL", "192.168.10.133", 105)

        IO :
            println("Today's CT exams")
            println("-" * 80)
        *> 
        r.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, 
            Some(StudyLevel), dtag, DicomBase.printTags("euc_kr")).as(ExitCode.Success)

//> using repository https://maven.scijava.org/content/repositories/public
//> using dep org.dcm4che:dcm4che-core:5.30.0
//> using dep org.dcm4che:dcm4che-net:5.30.0
//> using dep org.dcm4che:dcm4che-imageio:5.30.0
//> using dep org.slf4j:slf4j-simple:2.0.7
import javax.lang.model.element.Element

// DIMSE-C 
// https://dicom.nema.org/medical/dicom/current/output/chtml/part07/chapter_9.html

System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "error")
println("Starting...")

// import $ivy.`org.dcm4che:dcm4che-core:5.30.0`
// import $ivy.`org.dcm4che:dcm4che-net:5.30.0`
import org.dcm4che3.data.*
import org.dcm4che3.net.*
import java.util.concurrent.{Executors, ExecutorService}

val device = Device("READROOM")
val conn = Connection()

val remote1 = Connection()
remote1.setHostname("192.168.10.133")
remote1.setPort(105)

val remote2 = Connection()
remote2.setHostname("192.168.10.130")
remote2.setPort(204)

val executorService = Executors.newSingleThreadExecutor()
val scheduledExecutorService = Executors.newSingleThreadScheduledExecutor()
val ae = ApplicationEntity("READROOM")

device.addConnection(conn)
device.addApplicationEntity(ae)
ae.addConnection(conn)

ae.setAETitle("READROOM")

conn.setReceivePDULength(Connection.DEF_MAX_PDU_LENGTH)
conn.setSendPDULength(Connection.DEF_MAX_PDU_LENGTH)
conn.setMaxOpsInvoked(0)
conn.setMaxOpsPerformed(0)
conn.setPackPDV(true)
conn.setConnectTimeout(0)
conn.setRequestTimeout(0)
conn.setAcceptTimeout(0)
conn.setReleaseTimeout(0)
conn.setSendTimeout(0)
conn.setStoreTimeout(0)
conn.setResponseTimeout(0)
conn.setIdleTimeout(0)
conn.setSocketCloseDelay(Connection.DEF_SOCKETDELAY)
conn.setSendBufferSize(0)
conn.setReceiveBufferSize(0)
conn.setTcpNoDelay(true)

device.setExecutor(executorService)
device.setScheduledExecutor(scheduledExecutorService)

import scala.jdk.CollectionConverters.*

def printTags(index: Int, data: Attributes, preTag: Seq[String] = Seq.empty): Unit = 
    data.tags().foreach(tag =>
        val tstr = String.format("0x%08x", tag)
        val vr = ElementDictionary.vrOf(tag, data.getPrivateCreator(tag))
        if vr == VR.SQ then
            data.getSequence(tag).asScala.foreach(attr => printTags(index, attr, preTag :+ s"${tstr}"))
        else
            println(s"#$index(${(preTag :+ tstr).mkString(".")})[${ElementDictionary.keywordOf(tag, null)}] ${String(data.getBytes(tag),"euc-kr")}")
    )

import pdu.{AAssociateRQ, PresentationContext}
import org.dcm4che3.data.UID

// C-ECHO
println("C-ECHO")
println("-"*40)

val req1 = AAssociateRQ()
req1.setCalledAET("NETGEAR_EXTERNAL")
req1.addPresentationContext(PresentationContext(1, UID.Verification, UID.ImplicitVRLittleEndian, UID.ExplicitVRLittleEndian, UID.ExplicitVRBigEndian))

val as1 = ae.connect(conn, remote1, req1)
val rsp1 = as1.cecho()
rsp1.next()
val attrs1 = rsp1.getCommand()
// Tag.Status == 0x00000900
val rstring = if attrs1.contains(0x00000900) && attrs1.getInt(0x00000900, -1) == 0 then "Successful C-ECHO" else "Failed C-ECHO"
println(rstring)

as1.waitForOutstandingRSP()
as1.release()
as1.waitForSocketClose()

// C-FIND of ModalityWorkList
println("MODALITY WORKLIST")
println("-"*40)

val req2 = AAssociateRQ()
req2.setCalledAET("WORKGATE")
req2.addPresentationContext(PresentationContext(1, UID.ModalityWorklistInformationModelFind, UID.ImplicitVRLittleEndian, UID.ExplicitVRLittleEndian, UID.ExplicitVRBigEndian))

val attrs2 = Attributes()
val item2 = attrs2.newSequence(0x00400100,1)  // Tag.ScheduledProcedureStepSequence
val subattr2 = Attributes()
item2.add(subattr2)
Seq((0x00400002, "20230811"), (0x00080060, "US")).foreach((tag, value) => 
    val vr = ElementDictionary.vrOf(tag, subattr2.getPrivateCreator(tag))
    subattr2.setString(tag, vr, value)
)
Seq(Tag.PatientID, Tag.PatientName).foreach(tag => 
    val vr = ElementDictionary.vrOf(tag, subattr2.getPrivateCreator(tag))
    attrs2.setNull(tag, vr))
println(attrs2)

// MWL에서는 QueryRetrieveLevel 없다
// attrs.setString(Tag.QueryRetrieveLevel, VR.CS, )

io.StdIn.readLine()
val as2 = ae.connect(conn, remote2, req2)
// val jobs = scala.collection.mutable.ArrayBuffer.empty[IO[_]]

val dimseRespHandler2 = new DimseRSPHandler(as2.nextMessageID()):
    var count = 0
    override def onDimseRSP(as: Association, cmd: Attributes, data: Attributes) = 
        super.onDimseRSP(as, cmd, data)
        val status = cmd.getInt(Tag.Status, -1)
        if Status.isPending(status) then
            // jobs += IO.blocking {
            //     println("YOI! - ")
            //     printTags(data)
            // }
            count += 1
            printTags(count, data)

try
    as2.cfind(UID.ModalityWorklistInformationModelFind, Priority.NORMAL, attrs2, null, dimseRespHandler2)
finally
    as2.waitForOutstandingRSP()
    as2.release()
    as2.waitForSocketClose()
    // jobs.toList.sequence.unsafeRunSync()

println("TODAY'S CT EXAMS")
println("-"*40)

val req3 = AAssociateRQ()
req3.setCalledAET("NETGEAR_EXTERNAL")
req3.addPresentationContext(PresentationContext(1, UID.StudyRootQueryRetrieveInformationModelFind, UID.ImplicitVRLittleEndian, UID.ExplicitVRLittleEndian, UID.ExplicitVRBigEndian))

val attrs3 = Attributes()
Seq((Tag.StudyDate, "20230811"), (Tag.ModalitiesInStudy, "CT")).foreach((tag, value) => 
    val vr = ElementDictionary.vrOf(tag, null)
    attrs3.setString(tag, vr, value)
)
Seq(Tag.PatientID, Tag.PatientName, Tag.StudyDescription, Tag.StudyInstanceUID).foreach(tag => 
    val vr = ElementDictionary.vrOf(tag, null)
    attrs3.setNull(tag, vr))
attrs3.setString(Tag.QueryRetrieveLevel, VR.CS, "STUDY")
println(attrs3)
/*
var firstSIU: String = _

def mkDimseRespHandler(msgId: Int) = new DimseRSPHandler(msgId):
    var count = 0
    override def onDimseRSP(as: Association, cmd: Attributes, data: Attributes) = 
        super.onDimseRSP(as, cmd, data)
        val status = cmd.getInt(Tag.Status, -1)
        if Status.isPending(status) then
            count += 1
            if count == 1 then 
                firstSIU = data.getString(Tag.StudyInstanceUID)
            printTags(count, data)
*/

var lastUID: String = _

// C-FIND에서는 cmd에 NumberOf... 정보가 없다
def mkOddResHandler(msgId: Int, qrLevel: String) = 
    val rTag = qrLevel.toUpperCase() match
        case "STUDY" => Tag.StudyInstanceUID
        case "SERIES" => Tag.SeriesInstanceUID
        case _ => Tag.SOPInstanceUID
    new DimseRSPHandler(msgId):
        override def onDimseRSP(as: Association, cmd: Attributes, data: Attributes) = 
            super.onDimseRSP(as, cmd, data)
            val status = cmd.getInt(Tag.Status, -1)
            if Status.isPending(status) then
                printTags(0, data)
                lastUID = data.getString(rTag)
            // if 문 바깥에 command 있으면 경우에 따라 exception 발생, 기록 / 처리는 꼭 isPending일때 하자

io.StdIn.readLine()
val as3 = ae.connect(conn, remote1, req3) 
val handler3 = mkOddResHandler(as3.nextMessageID(), "STUDY")
try
    as3.cfind(UID.StudyRootQueryRetrieveInformationModelFind, Priority.NORMAL, attrs3, null, handler3)
finally
    as3.waitForOutstandingRSP()
    as3.release()
    as3.waitForSocketClose()

println("GETTING TODAY'S LAST CT STUDY's SERIES")
println(s"StudyInstanceUID is '$lastUID'")
println("-"*40)

val attrs3_1 = Attributes()
Seq((Tag.StudyInstanceUID, lastUID)).foreach((tag, value) => 
    val vr = ElementDictionary.vrOf(tag, null)
    attrs3_1.setString(tag, vr, value)
)
Seq(Tag.SeriesDescription, Tag.SeriesNumber, Tag.SeriesInstanceUID).foreach(tag => 
    val vr = ElementDictionary.vrOf(tag, null)
    attrs3_1.setNull(tag, vr))
attrs3_1.setString(Tag.QueryRetrieveLevel, VR.CS, "SERIES")
println(attrs3_1)

io.StdIn.readLine()
val as3_1 = ae.connect(conn, remote1, req3)
try
    as3_1.cfind(UID.StudyRootQueryRetrieveInformationModelFind, Priority.NORMAL, attrs3_1, null, mkOddResHandler(as3_1.nextMessageID(), "SERIES"))
finally
    as3_1.waitForOutstandingRSP()
    as3_1.release()
    as3_1.waitForSocketClose()

println("GETTING TODAY'S LAST CT STUDY's IMAGES")
println(s"SeriesInstanceUID is '$lastUID'")
println("-"*40)

val attrs3_2 = Attributes()
Seq((Tag.SeriesInstanceUID, lastUID)).foreach((tag, value) => 
    val vr = ElementDictionary.vrOf(tag, null)
    attrs3_2.setString(tag, vr, value)
)
Seq(Tag.SOPInstanceUID).foreach(tag => 
    val vr = ElementDictionary.vrOf(tag, null)
    attrs3_2.setNull(tag, vr))
attrs3_2.setString(Tag.QueryRetrieveLevel, VR.CS, "IMAGE")
println(attrs3_2)

io.StdIn.readLine()
val as3_2 = ae.connect(conn, remote1, req3)
try
    as3_2.cfind(UID.StudyRootQueryRetrieveInformationModelFind, Priority.NORMAL, attrs3_2, null, mkOddResHandler(as3_2.nextMessageID(), "IMAGE"))
finally
    as3_2.waitForOutstandingRSP()
    as3_2.release()
    as3_2.waitForSocketClose()

val req4 = AAssociateRQ()
req4.setCalledAET("NETGEAR_EXTERNAL")
req4.addPresentationContext(PresentationContext(1, UID.Verification, UID.ImplicitVRLittleEndian, UID.ExplicitVRLittleEndian, UID.ExplicitVRBigEndian))
req4.addPresentationContext(PresentationContext(3, UID.StudyRootQueryRetrieveInformationModelGet, UID.ImplicitVRLittleEndian, UID.ExplicitVRLittleEndian, UID.ExplicitVRBigEndian))

/* from store-tcs.properties
CTImageStorage:\
ImplicitVRLittleEndian;ExplicitVRLittleEndian
*/
import pdu.RoleSelection

val role = "CTImageStorage"
val transferSyntaxes = Seq("ImplicitVRLittleEndian","ExplicitVRLittleEndian").map(UID.forName)
req4.addRoleSelection(RoleSelection(role, false, true))
req4.addPresentationContext(PresentationContext(
    2 * req4.getNumberOfPresentationContexts() + 1, UID.forName(role), transferSyntaxes:_*))

val attrs4 = Attributes()
attrs4.setString(Tag.QueryRetrieveLevel, VR.CS, "IMAGE")
val iuid = "1.2.392.200036.9116.2.6.1.3268.2047634826.1691560489.925603"
val iuid2 = "1.2.392.200036.9116.2.6.1.3268.2047634826.1691560489.231724"
val studyvr4 = ElementDictionary.vrOf(Tag.SOPInstanceUID, null)
attrs4.setString(Tag.SOPInstanceUID, studyvr4, lastUID)
println(attrs4)

import org.dcm4che3.net.service.*
import java.io.{File, OutputStream, FileOutputStream, BufferedOutputStream}
import org.dcm4che3.io.DicomOutputStream

val storageSCP = new BasicCStoreSCP("*"):
    override def store(as: Association, pc: PresentationContext, rq: Attributes,
        data: PDVInputStream, rsp: Attributes) = 
            println("*store.STORAGESCP")
            val iuid = rq.getString(Tag.AffectedSOPInstanceUID)
            val cuid = rq.getString(Tag.AffectedSOPClassUID)
            val tsuid = pc.getTransferSyntax()
            val metainfo = as.createFileMetaInformation(iuid, cuid, tsuid)
            val dout: OutputStream = BufferedOutputStream(FileOutputStream(File(iuid)))
            val dostream = DicomOutputStream(dout, tsuid)
            try
                dostream.writeFileMetaInformation(metainfo)
                data.copyTo(dostream)
            finally
                dostream.close()

/*
val serviceRegistry4 = DicomServiceRegistry()
serviceRegistry4.addDicomService(storageSCP)
device.setDimseRQHandler(serviceRegistry4)

val as4 = ae.connect(conn, remote1, req4)
val dimseRespHandler4 = new DimseRSPHandler(as4.nextMessageID()):
    override def onDimseRSP(as: Association, cmd: Attributes, data: Attributes) =
        super.onDimseRSP(as, cmd, data)

try 
    as4.cget(UID.StudyRootQueryRetrieveInformationModelGet, Priority.NORMAL, attrs4, null, dimseRespHandler4)
finally
    as4.waitForOutstandingRSP()
    as4.release()
    as4.waitForSocketClose()
*/

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import javax.imageio.ImageIO
import org.dcm4che3.io.DicomInputStream

// this import registers "DICOM" format reader to ImageIO
import org.dcm4che3.imageio.plugins.dcm.*   
val dicomReader = ImageIO.getImageReadersByFormatName("DICOM").next()

val buffs = collection.mutable.ArrayBuffer.empty[ByteArrayOutputStream]
val storageSCP2 = new BasicCStoreSCP("*"):
    var count = 0
    override def store(as: Association, pc: PresentationContext, rq: Attributes,
        data: PDVInputStream, rsp: Attributes) = 
            val iuid = rq.getString(Tag.AffectedSOPInstanceUID)
            val cuid = rq.getString(Tag.AffectedSOPClassUID)
            val tsuid = pc.getTransferSyntax()
            val metainfo = as.createFileMetaInformation(iuid, cuid, tsuid)
            val byteStream = ByteArrayOutputStream()
            val dostream = DicomOutputStream(byteStream, tsuid)
            try
                dostream.writeFileMetaInformation(metainfo)
                data.copyTo(dostream)
            finally
                dostream.close()
            buffs += byteStream
            count += 1
            println(s"*** STORED $count - ${buffs.length}")

val serviceRegistry5 = DicomServiceRegistry()
serviceRegistry5.addDicomService(storageSCP2)
device.setDimseRQHandler(serviceRegistry5)

io.StdIn.readLine()
println("MAKING CONNECTION")
val as5 = ae.connect(conn, remote1, req4)
val dimseRespHandler5 = new DimseRSPHandler(as5.nextMessageID()):
    override def onDimseRSP(as: Association, cmd: Attributes, data: Attributes) =
        super.onDimseRSP(as, cmd, data)
        // operation 관련 정보는 cmd에 포함되어 있음
        println(s"+++++DimseRSP $cmd")
        println(cmd.getInt(Tag.NumberOfCompletedSuboperations, -1))
        println(cmd.getInt(Tag.NumberOfRemainingSuboperations, -1))
        println(cmd.getInt(Tag.NumberOfFailedSuboperations, -1))
try 
    println("GET-COMMAND")
    as5.cget(UID.StudyRootQueryRetrieveInformationModelGet, Priority.NORMAL, attrs4, null, dimseRespHandler5)
finally
    as5.waitForOutstandingRSP()
    as5.release()
    as5.waitForSocketClose()

println("Now, converting...")
buffs.zipWithIndex.foreach((bs, i) => 
    val dis = DicomInputStream(ByteArrayInputStream(bs.toByteArray()))
    // val attr = dis.readDataset()
    dicomReader.setInput(dis)
    val im = dicomReader.read(0, dicomReader.getDefaultReadParam())            
    // val accNo = attr.getString(Tag.StudyInstanceUID)
    val fname = java.nio.file.Paths.get(".", s"FIRSTCT-$i.png")
    javax.imageio.ImageIO.write(im, "png", fname.toFile())
)

executorService.shutdown()
scheduledExecutorService.shutdown()
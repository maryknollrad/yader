package net.maryknollrad.d4cs

import org.dcm4che3.data.*
import org.dcm4che3.io.DicomInputStream
import org.dcm4che3.io.DicomOutputStream
import org.dcm4che3.net.*
import org.dcm4che3.net.pdu.AAssociateRQ
import org.dcm4che3.net.pdu.ExtendedNegotiation
import org.dcm4che3.net.pdu.PresentationContext
import org.dcm4che3.net.pdu.RoleSelection
import org.dcm4che3.net.service.BasicCStoreSCP
import org.dcm4che3.net.service.DicomServiceException
import org.dcm4che3.net.service.DicomServiceRegistry
import org.dcm4che3.util.SafeClose
import org.dcm4che3.util.StringUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import java.io.File
import java.io.IOException
import java.io.OutputStream
import java.security.GeneralSecurityException
import java.text.MessageFormat
import java.util.List
import java.util.Map.Entry
import java.util.Properties
import java.util.ResourceBundle
import java.util.Set
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit

import cats.effect.kernel.Resource
import cats.effect.IO
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import org.dcm4che3.util.ResourceLocator
import java.io.ByteArrayOutputStream
import java.awt.Image

import DicomBase.*
import RetrieveLevel.*

trait DicomCGetBase {
    val deviceName: String
    val callingAe: String
    val calledAe: String
    val remoteHost: String
    val remotePort: Int
    val hostName: String
    val storeFile: Boolean 

    private val device = Device(deviceName)
    // GetSCU constructor에서 초기화
    private val ae = ApplicationEntity(callingAe)
    private val conn = Connection()
    private val remote = Connection()
    private val rq = AAssociateRQ()
    private val keys = Attributes()

    private val priority = Priority.NORMAL
    // private val as: Association = ???
    // GetSCU.setCancelAfter, default value
    private val cancelAfter = 0     

    private val executorService = Executors.newSingleThreadExecutor()
    private val scheduledExecutorService = Executors.newSingleThreadScheduledExecutor()

    // memory storage, key => SOPInstanceUID
    private val imageStorage = scala.collection.mutable.Map.empty[String, ByteArrayOutputStream]

    private val storageSCP = new BasicCStoreSCP("*") {
        override def store(as: Association, pc: PresentationContext, rq: Attributes,
            data: PDVInputStream, rsp: Attributes) = {
                val iuid = rq.getString(Tag.AffectedSOPInstanceUID)
                val cuid = rq.getString(Tag.AffectedSOPClassUID)
                val tsuid = pc.getTransferSyntax()
                val metainfo = as.createFileMetaInformation(iuid, cuid, tsuid)

                // IO 반환되어 처리되지 않음?
                /*
                val dOutResource = getOutputStream(iuid, tsuid)
                dOutResource.use { dout => 
                    println(s"saving SOPInstanceUID $iuid")
                    // IO(println(s"saving SOPInstanceUID $iuid")) *>
                    IO({
                        dout.writeFileMetaInformation(metainfo)
                        data.copyTo(dout)
                    })}
                */

                val dout: OutputStream = 
                    if (storeFile) then
                        BufferedOutputStream(FileOutputStream(File(iuid)))
                    else
                        val bs = ByteArrayOutputStream()
                        imageStorage += (iuid -> bs)
                        bs
                val dostream = DicomOutputStream(dout, tsuid)
                try {
                    dostream.writeFileMetaInformation(metainfo)
                    data.copyTo(dostream)
                } finally {
                    dostream.close()
                }
            }
    }

    // combination of GetSCU.storeTo function
    private def getOutputStream(iuid: String, tsuid: String) = {
        val output: OutputStream = 
            if (storeFile)
                BufferedOutputStream(FileOutputStream(File(iuid)))
            else
                ByteArrayOutputStream()
        Resource.make(IO(DicomOutputStream(output, tsuid)))(os => IO(os.close()))
    }


    /* GetSCU constructor */
    device.addConnection(conn)
    device.addApplicationEntity(ae)
    ae.addConnection(conn)
    val serviceRegistry = DicomServiceRegistry()
    serviceRegistry.addDicomService(storageSCP)
    device.setDimseRQHandler(serviceRegistry)

    /* GetSCU.main */
    /* CLIUtil.configureConnect */
    remote.setHostname(remoteHost)
    remote.setPort(remotePort)
    // rq.setCalledAE(??) => makeRequest
    // conn.setHttpProxy()

    /* CLIUtil.configureBind */
    ae.setAETitle(callingAe)
    // conn.setHostname(hostName)
    // conn.setPort()

    /* CLIUtil.configure */
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
    
    /* skipped CLIUtil.configureTLS - no TLS */
    // remote.setTlsProtocols(conn.getTlsProtocols)
    // remote.setTlsCipherSuites(conn.getTlsCipherSuites)

    // get 호출때마다 새로운 request 생성 - 원래의 rq 변수 대치
    private def makeRequest(cuid: String) = {
        val ivr_le_only = Seq(UID.ImplicitVRLittleEndian, UID.ExplicitVRLittleEndian, UID.ExplicitVRBigEndian)
        val request = AAssociateRQ()
        request.setCalledAET(calledAe)
        request.addPresentationContext(PresentationContext(1, cuid, ivr_le_only:_*))
        request
    }

    /* configureServiceClass */
    // setInformationModel(informationModel, 
    //      transferSyntax (EVR_LE_FIRST, EVR_BE_FIRST, IVR_LE_ONLY, IVR_LE_FIRST[기본]), 
    //      enumSet) => makeRequest 함수로 처리
    // command line option (store-tc, storetcs)에 따라 configureStorageSOPClass 처리 => skip
    def configureServiceClass(request: AAssociateRQ, level: RetrieveLevel) = {
        def toUID(s: String) = {
            val trimmed = s.trim()
            if (trimmed == "*" || trimmed(0).isDigit) trimmed else UID.forName(trimmed)
        }
        keys.clear()
        keys.setString(Tag.QueryRetrieveLevel, VR.CS, level.strRepr)
        
        val properties = Properties()
        val pstream = ResourceLocator.getResourceURL("store-tcs.properties", classOf[DicomCGetBase]).openStream()
        try { properties.load(pstream) }
        finally { pstream.close() }

        // https://www.generacodice.com/en/articolo/335046/Getting+a+Scala+Map+from+a+Java+Properties
        import scala.collection.convert.ImplicitConversions.`properties AsScalaMap`
        val pmap: scala.collection.mutable.Map[String, String] = properties

        pmap.foreach({ case ((cuid, tsuidStr)) => 
            val tsuids = tsuidStr.split(";").map(toUID)
            if (!request.containsPresentationContextFor(cuid))
                request.addRoleSelection(RoleSelection(cuid, false, true))
            request.addPresentationContext(PresentationContext(
                2 * request.getNumberOfPresentationContexts() + 1, toUID(cuid), tsuids:_*))
        })
    }

    /* configureKeys */
    // CLI.addAttributes는 .으로 구분된 하나의 attribute를 추가하는 데 사용된다
    // int []tags로 되어있지만 여럿의 tag가 아니라 계층별 분리된 하나의 tag
    def addAttributes(tagHierarchy: Seq[Int], value: Option[String]) = {
        var item: Attributes = keys
        tagHierarchy.init.foreach({ tag => 
            val pre_sq = item.getSequence(tag)
            val sq = if (pre_sq == null) item.newSequence(tag, 1) else pre_sq
            if (sq.isEmpty) sq.add(Attributes())
            item = sq.get(0)
        })
        val tag = tagHierarchy.last
        val vr = ElementDictionary.vrOf(tag, item.getPrivateCreator(tag))
        value match {
            case Some(tv) => 
                item.setString(tag, vr, tv)
            case None => 
                if (vr == VR.SQ) item.newSequence(tag, 1).add(Attributes(0))
                else item.setNull(tag, vr)
        }
    }

    // write retrieve keys first, overwrite query keys later
    def addKeys(queryKeys: Map[Int, String], retrieveKeys: Seq[Int]) = {
        retrieveKeys.foreach(tag => addAttributes(Seq(tag), None))
        queryKeys.foreach((tag, value) => addAttributes(Seq(tag), Some(value)))
    }

    /* configureStorageDirectory */ 

    /* skipped configureCancel */

    /* main.setPriority */
    device.setExecutor(executorService)
    device.setScheduledExecutor(scheduledExecutorService)

    /* main.open */
    def getAssociation(request: AAssociateRQ) = Resource.make
            (IO(ae.connect(conn, remote, request)))({ case as:Association => 
            IO({ 
                as.waitForOutstandingRSP()
                as.release()
                // executorService.shutdown()
                // scheduledExecutorService.shutdown()
            }) })

    def configureServiceClass(req: AAssociateRQ) = ???
    def getStudy(cuid: String, level: Option[RetrieveLevel], tags: DicomTags) = {
        // 기본 StudyRoot - GetSCU.informationModelOf
        println(s"GETSTUDY : $cuid - $tags")
        val request = makeRequest(cuid)
        configureServiceClass(request, level.getOrElse(StudyLevel)) 
        // addKeys(tags.query, tags.retrieve)
        getAssociation(request).use { as => IO({
            val dimpseRespHandler = new DimseRSPHandler(as.nextMessageID()) {
                override def onDimseRSP(as: Association, cmd: Attributes, data: Attributes) = {
                    super.onDimseRSP(as, cmd, data)
                }
            }
            as.cget(cuid, priority, keys, null, dimpseRespHandler)
        })}} 

    def getImageStorage() = imageStorage

    def shutdown() = 
        executorService.shutdown()
        scheduledExecutorService.shutdown()
}

/*
object DicomGet:
    private val deviceName = "scala-d4c"
    private val hostName = "GETSCU"

    def resource(callingAe: String, calledAe: String, remoteHost: String, remotePort: Int, storeToFile: Boolean = true) = 
        Resource.make(IO(DicomGet(deviceName, hostName, callingAe, calledAe, remoteHost, remotePort, storeToFile)))(dg => IO(dg.shutdown()))
    
case class DicomGet(deviceName: String, hostName: String, callingAe: String, calledAe: String, 
    remoteHost: String, remotePort: Int, storeFile: Boolean) extends DicomCGetBase:
    def getStudyByStudyUid(suid: String) = 
        getStudy(UID.StudyRootQueryRetrieveInformationModelGet, Some(StudyLevel), DicomQuery.studyQueryTags(suid))
    
    def getInstanceByInstanceUid(stuid: String, seuid: String, uid: String) = 
        val qts = DicomQuery.imageQueryTags(stuid, seuid, uid)
        println(qts)
        getStudy(UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), qts)
*/
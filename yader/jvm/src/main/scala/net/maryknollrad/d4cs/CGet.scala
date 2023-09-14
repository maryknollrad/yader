package net.maryknollrad.d4cs

import org.dcm4che3.data.*
import org.dcm4che3.io.{DicomInputStream, DicomOutputStream}
import org.dcm4che3.net.*
import org.dcm4che3.net.pdu.{AAssociateRQ, ExtendedNegotiation, PresentationContext, RoleSelection}
import org.dcm4che3.net.service.{BasicCEchoSCP, BasicCStoreSCP, DicomServiceException, DicomServiceRegistry}
// import org.dcm4che3.util.SafeClose
import org.dcm4che3.util.StringUtils
import org.slf4j.{Logger, LoggerFactory}

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

// USING C-GET RATHER THAN C-MOVE 
// http://dclunie.blogspot.com/2016/05/to-c-move-is-human-to-c-get-divine.html
case class CGet(val callingAe: String, val calledAe: String, val remoteHost: String, val remotePort: Int, val storeFile: Boolean = true) extends DicomBase:
    val deviceName: String = "GETSCU"

    private val device = Device(deviceName)
    // GetSCU constructor에서 초기화
    private val ae = ApplicationEntity(callingAe)
    private val conn = Connection()
    private val remote = Connection()
    private val rq = AAssociateRQ()

    private val priority = Priority.NORMAL
    // private val as: Association = ???
    // GetSCU.setCancelAfter, default value
    private val cancelAfter = 0     

    private val executorService = Executors.newSingleThreadExecutor()
    private val scheduledExecutorService = Executors.newSingleThreadScheduledExecutor()

    // memory storage, key => SOPInstanceUID
    private val imageStorage = scala.collection.mutable.Map.empty[String, ByteArrayOutputStream]
    private val logger = LoggerFactory.getLogger(getClass)

    private val storageSCP = new BasicCStoreSCP("*"):
        override def store(as: Association, pc: PresentationContext, rq: Attributes,
            data: PDVInputStream, rsp: Attributes) = 
                logger.debug("StorageSCP.store")
                val iuid = rq.getString(Tag.AffectedSOPInstanceUID)
                val cuid = rq.getString(Tag.AffectedSOPClassUID)
                val tsuid = pc.getTransferSyntax()
                val metainfo = as.createFileMetaInformation(iuid, cuid, tsuid)
                val dout: OutputStream = 
                    if (storeFile) then
                        BufferedOutputStream(FileOutputStream(File(iuid)))
                    else
                        val bs = ByteArrayOutputStream()
                        imageStorage += (iuid -> bs)
                        bs
                val dostream = DicomOutputStream(dout, tsuid)
                try
                    dostream.writeFileMetaInformation(metainfo)
                    data.copyTo(dostream)
                finally
                    dostream.close()

    /* GetSCU constructor */
    device.addConnection(conn)
    device.addApplicationEntity(ae)
    ae.addConnection(conn)
    val serviceRegistry = DicomServiceRegistry()
    serviceRegistry.addDicomService(new BasicCEchoSCP())
    serviceRegistry.addDicomService(storageSCP)
    device.setDimseRQHandler(serviceRegistry)   // DimeseRQHandler is here!

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
    private def makeRequest(cuid: String) = 
        val ivr_le_only = Seq(UID.ImplicitVRLittleEndian, UID.ExplicitVRLittleEndian, UID.ExplicitVRBigEndian)
        val request = AAssociateRQ()
        request.setCalledAET(calledAe)
        request.addPresentationContext(PresentationContext(1, cuid, ivr_le_only:_*))
        request

    /* configureServiceClass */
    // setInformationModel(informationModel, 
    //      transferSyntax (EVR_LE_FIRST, EVR_BE_FIRST, IVR_LE_ONLY, IVR_LE_FIRST[기본]), 
    //      enumSet) => makeRequest 함수로 처리
    // command line option (store-tc, storetcs)에 따라 configureStorageSOPClass 처리 => skip
    def configureServiceClass(request: AAssociateRQ, level: RetrieveLevel) = 
        def toUID(s: String) = 
            val trimmed = s.trim()
            if (trimmed == "*" || trimmed(0).isDigit) trimmed else UID.forName(trimmed)
        keys.clear()
        keys.setString(Tag.QueryRetrieveLevel, VR.CS, level.strRepr)
        
        val properties = Properties()
        val pstream = ResourceLocator.getResourceURL("store-tcs.properties", classOf[CGet]).openStream()
        try 
            properties.load(pstream)
        finally 
            pstream.close() 

        // https://www.generacodice.com/en/articolo/335046/Getting+a+Scala+Map+from+a+Java+Properties
        import scala.collection.convert.ImplicitConversions.`properties AsScalaMap`
        val pmap: scala.collection.mutable.Map[String, String] = properties

        pmap.foreach:
            case ((cuid, tsuidStr)) => 
                val tsuids = tsuidStr.split(";").map(toUID)
                if (!request.containsPresentationContextFor(cuid))
                    request.addRoleSelection(RoleSelection(cuid, false, true))
                request.addPresentationContext(PresentationContext(
                    2 * request.getNumberOfPresentationContexts() + 1, toUID(cuid), tsuids:_*))

    /* configureStorageDirectory */ 

    /* skipped configureCancel */

    /* main.setPriority */
    device.setExecutor(executorService)
    device.setScheduledExecutor(scheduledExecutorService)

    /* main.open */
    def getAssociation(request: AAssociateRQ) = Resource.make
            (IO.blocking(ae.connect(conn, remote, request)))({ case as:Association => 
                IO.blocking:
                    as.waitForOutstandingRSP()
                    as.release()
            })

    // def configureServiceClass(req: AAssociateRQ) = ???
    def getStudy(cuid: String, level: Option[RetrieveLevel], tags: DicomTags) = 
        // 기본 StudyRoot - GetSCU.informationModelOf
        // println(s"GETSTUDY : $cuid - $tags")
        val request = makeRequest(cuid)
        configureServiceClass(request, level.getOrElse(StudyLevel))
        addKeys(tags)
        getAssociation(request).use: as => 
            IO.blocking:
                val dimseRespHandler = new DimseRSPHandler(as.nextMessageID()):
                    override def onDimseRSP(as: Association, cmd: Attributes, data: Attributes) =
                        super.onDimseRSP(as, cmd, data)
                as.cget(cuid, priority, keys, null, dimseRespHandler)

    def getDicomInputStreamAndFree(sopInstanceUid: String) = 
        val k = sopInstanceUid.trim
        val r = imageStorage.get(k).map(ba => 
                    DicomInputStream(java.io.ByteArrayInputStream(ba.toByteArray())))
                .toRight(s"Cannot find SOPInstanceUID in memory $sopInstanceUid, currently holing ${imageStorage.size} items")
        imageStorage -= k
        r

    def shutdown() = 
        executorService.shutdown()
        scheduledExecutorService.shutdown()
package net.maryknollrad.d4cs

import org.dcm4che3.data.*
import org.dcm4che3.net.*
import org.dcm4che3.net.pdu.AAssociateRQ
import org.dcm4che3.net.pdu.ExtendedNegotiation
import org.dcm4che3.net.pdu.PresentationContext
import org.dcm4che3.util.SafeClose

import java.io.*
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.atomic.AtomicInteger
import java.time.{LocalDate, LocalTime}

import cats.effect.kernel.Resource
import cats.effect.IO
import cats.implicits.*
import javax.lang.model.element.Element
import java.time.format.DateTimeFormatter
import scala.util.{Try, Success, Failure}

import DicomBase.* 
import scala.collection.mutable.ArrayBuffer
import scala.util.chaining.* 
import org.slf4j.{Logger, LoggerFactory}

// rough adaption of dcm4che FindSCU 
// https://github.com/dcm4che/dcm4che/blob/master/dcm4che-tool/dcm4che-tool-findscu/src/main/java/org/dcm4che3/tool/findscu/FindSCU.java

case class CFind(val callingAe: String, val calledAe: String, val remoteHost: String, val remotePort: Int, val encoding: String = "utf-8") extends DicomBase:
    val deviceName: String = "FINDSCU"

    private val device = Device(deviceName)
    private val ae = ApplicationEntity(callingAe)
    private val conn = Connection()
    private val remote = Connection()

    private val priority = Priority.NORMAL
    
    private val executorService = Executors.newSingleThreadExecutor()
    private val scheduledExecutorService = Executors.newSingleThreadScheduledExecutor()

    private val logger = LoggerFactory.getLogger(getClass)

    /* FindSCU.main */

    /* FindSCU constructor */
    device.addConnection(conn)
    device.addApplicationEntity(ae)
    ae.addConnection(conn)

    /* CLIUtil.configureConnect */
    remote.setHostname(remoteHost)
    remote.setPort(remotePort)
    // remote.setHttpProxy()

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

    // customn setup - query 함수에 maxNum을 인자로 주었을 때 cancel 호출하면 ApplicationEntity에 dimseRQHandler가 호출돠며 지정하지 않으면 없다고 에러 log
    private val dimseRQHandler = new DimseRQHandler():
        override def onDimseRQ(as: Association, pc: PresentationContext, dimse: Dimse, cmd: Attributes, data: PDVInputStream) = 
            logger.debug("\n\n+++DimseRQHandler.onDimseRQ : {}, {}, {}, {}, {}", as, pc, dimse, cmd, data)
        override def onClose(as: Association) = 
            logger.debug("\n\n+++DimseRQHandler.onClose : {}", as)
    ae.setDimseRQHandler(dimseRQHandler)

    // query 호출때마다 새로운 request 생성 - 원래의 rq 변수 대치
    // request를 private 변수로 바꾸면 Already contains Presentation Context with pid: 1
    // CONSIDER : cuid별 request를 하나씩만 만들기?
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
    //      enumSet) => 함수로 처리
    def setInformationModel(cuid: String, level: Option[RetrieveLevel]) = {
        keys.clear()
        level.foreach(l => keys.setString(Tag.QueryRetrieveLevel, VR.CS, l.strRepr))
    }

    /* skipped configureOutput */ 
    /* skipped configureCancel */

    /* main.setPriority */
    device.setExecutor(executorService)
    device.setScheduledExecutor(scheduledExecutorService)

    /* main.open */
    def getAssociation(request: AAssociateRQ) = 
        Resource.make { 
                IO.blocking :
                    ae.connect(conn, remote, request)
            }{ case as:Association => 
                IO.blocking :
                    logger.debug("+++ Freeing association : {}", as)
                    as.waitForOutstandingRSP()
                    as.release() 
                    // 여러번 호출하려면 shutdown 하면 안 됨, 언제 shutdown??
                    // executorService.shutdown()
                    // scheduledExecutorService.shutdown()
            }

    def toTagArray(dtags: DicomTags, attr: Attributes): Seq[StringTag] =
        dtags.map: tag =>
            tag match 
                case t: QueryTag =>
                    t
                case t: RetrieveTag =>
                    StringTag(t, DicomTags.getStringTag(t, attr, encoding))

    /* Idle something exception happens when QueryHandler function takes some time, 
       use toTagArray function to get tag and then use the data after handler returns */
    def query[A](cuid: String, level: Option[RetrieveLevel], 
            dtags: DicomTags, maxNum: Option[Int]= None, handler: QueryHandler[A] = toTagArray): IO[Seq[A]] = 
        setInformationModel(cuid, level)
        val request = makeRequest(cuid)
        addKeys(dtags)
        logger.info("\n\n***CFIND keys before open : {}", keys)
        val result = ArrayBuffer.empty[A]
        var numMatches: Int = 0
        for 
            _ <- getAssociation(request).use: as => 
                IO.blocking :
                    val dimseRespHandler = new DimseRSPHandler(as.nextMessageID()):
                        override def onDimseRSP(as: Association, cmd: Attributes, data: Attributes) = 
                            super.onDimseRSP(as, cmd, data)
                            val status = cmd.getInt(Tag.Status, -1)
                            if Status.isPending(status) then
                                result += handler(dtags, data) // .tap(a => log.info(s"\n\n***CFIND handled data {}\n", a))
                            maxNum.foreach(m => 
                                numMatches += 1
                                if numMatches >= m then 
                                    cancel(as)
                                    logger.debug(s"\n\n***CANCEL CALLED in CFind {} matches", numMatches)
                            )
                    as.cfind(cuid, priority, keys, null, dimseRespHandler)
        yield result.toSeq
    
    def shutdown() = 
        executorService.shutdown()
        scheduledExecutorService.shutdown()
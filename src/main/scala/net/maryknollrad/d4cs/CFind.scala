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

// rough adaption of dcm4che FindSCU 
// https://github.com/dcm4che/dcm4che/blob/master/dcm4che-tool/dcm4che-tool-findscu/src/main/java/org/dcm4che3/tool/findscu/FindSCU.java
case class CFind(val callingAe: String, val calledAe: String, val remoteHost: String, val remotePort: Int) extends DicomBase:
    val deviceName: String = "FINDSCU"

    private val device = Device(deviceName)
    private val ae = ApplicationEntity(callingAe)
    private val conn = Connection()
    private val remote = Connection()

    private val priority = Priority.NORMAL
    
    private val executorService = Executors.newSingleThreadExecutor()
    private val scheduledExecutorService = Executors.newSingleThreadScheduledExecutor()

    /* FindSCU constructor */
    device.addConnection(conn)
    device.addApplicationEntity(ae)
    ae.addConnection(conn)

    /* FindSCU.main */
    /* CLIUtil.configureConnect */
    remote.setHostname(remoteHost)
    remote.setPort(remotePort)

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

    // query 호출때마다 새로운 request 생성 - 원래의 rq 변수 대치
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
    def getAssociation(request: AAssociateRQ) = Resource.make
        (IO(ae.connect(conn, remote, request)))
        ({ case as:Association => 
            IO({
                as.waitForOutstandingRSP()
                as.release() 
                // 여러번 호출하려면 shutdown 하면 안 됨, 언제 shutdown??
                // executorService.shutdown()
                // scheduledExecutorService.shutdown()
            })})

    def query[A](cuid: String, level: Option[RetrieveLevel], 
            dtags: DicomTags, // querykeys: Map[Int, String], retrieve: Seq[Int], 
            handler: QueryHandler[A]) = 
        setInformationModel(cuid, level)
        val request = makeRequest(cuid)
        addKeys(dtags)
        // addKeys(dtags.query, dtags.retrieve)
        val result = scala.collection.mutable.ArrayBuffer.empty[A]
        for {
            _ <- getAssociation(request).use { as => IO({
                    val dimpseRespHandler = new DimseRSPHandler(as.nextMessageID()) {
                        override def onDimseRSP(as: Association, cmd: Attributes, data: Attributes) = {
                            super.onDimseRSP(as, cmd, data)
                            val status = cmd.getInt(Tag.Status, -1)
                            if (Status.isPending(status)) result += handler(dtags, data)
                        }
                    }
                    as.cfind(cuid, priority, keys, null, dimpseRespHandler)
                })}
            } yield result.toSeq
    
    def shutdown() = 
        executorService.shutdown()
        scheduledExecutorService.shutdown()
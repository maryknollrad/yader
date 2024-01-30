import cats.effect.*
import cats.syntax.all.*
import org.dcm4che3.data.*
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import net.maryknollrad.d4cs.*
import javax.imageio.ImageIO
import org.dcm4che3.io.DicomInputStream
import net.sourceforge.tess4j.*
import org.apache.commons.text.StringEscapeUtils

object YaderConf extends IOApp:
    private def config(host: String, port: Int, called: String, calling: String, encoding: String, tpath: String, insts: Seq[String]) = 
        s"""host = "$host"
        |port = $port
        |called-ae = "$called"
        |calling-ae = "$calling"
        |# encoding of PACS system 
        |encoding = "$encoding"
        |
        |# optional, if not given use SQLite with file name of 'yader.db'
        |# postgres-db = ""
        |# postgres-user = ""
        |# postgres-password = ""
        |
        |# install path of tesseract
        |tesseract-path = "${StringEscapeUtils.escapeJava(tpath)}"
        |# to filter other hospital's exam, multiple string values are supported
        |institution = [${insts.map(inst => s"\"$inst\"").mkString(",")}]
        |# dose value is ct or DLP
        |doseDLP = true
        |
        |# optional, if not exists starts from today
        |# process-begin = "2024-01-01"
        |
        |# optional, default 1, must be >= 0, if zero than store until today's exam else (today - store-gap) day
        |# process-day-behind = 1
        |
        |# optional, default 0, interval in seconds between each day's job processing
        |pause-interval = 10
        |
        |# optional, cron mode - runs in server mode
        |calendar-event = "*-*-* 08:30:00"
        |
        |# optional, stores png images of dose report series to (value of store-png-path) / yyyymmdd / accessionNumber.png with extracted dose value
        |# store-png-path = "doseimg"
        |
        |# web server port number, administrator pemission is needed to use port number less than 1024
        |# web-port-number = 7878
        |
        |# ip filter for editing drl value change
        |# empty list permits editing from any ips
        |# drl-edit-ips = []
        |
        |# permits editing from current server
        |drl-edit-ips = ["::1"]
        |
        |# show studies unassigned as 'NONE' in DRL tab
        |show-none = false
        |
        |# set default drl category, first category if not specified
        |# default-drl-category = "korea" 
        |""".stripMargin

    private def printHello = 
        IO.println("Yader configuration utility\n\n")
    
    import Console.*
    private def getServer = 
        for
            ip          <- printAndRead("Enter server address", Some(isIPAddr))
            port        <- printAndRead("Enter port number", Some(isNumber))
            called_ae   <- printAndRead("Enter server's Application Entity", Some(lengthInRange(1 to 16)))
            calling_ae  <- printAndRead("Enter application's Application Entity", Some(lengthInRange(1 to 16)), Some("YADER"))
            encoding    <- printAndRead("Enter the encoding of the PACS system", Some(nonEmpty), Some("utf-8"))
        yield (ip, port.toInt, called_ae, calling_ae, encoding)

    import net.maryknollrad.d4cs.RetrieveLevel.*
    private def todaysCTs(using cfind: CFind) = 
        import DicomBase.LocalDate2String
        val today = LocalDate.now()
        val dtags = DicomTags((Tag.ModalitiesInStudy, "CT"), (Tag.StudyDate, today:String),
                        Tag.StudyTime, Tag.PatientID, Tag.StudyDescription, Tag.StudyInstanceUID)
        cfind.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(StudyLevel), dtags)

    private def findSeries(studyUid: String, maxNum: Option[Int] = None)(using cfind: CFind) = 
        val dtags = DicomTags((Tag.StudyInstanceUID, studyUid), Tag.SeriesDescription, Tag.SeriesNumber, Tag.SeriesInstanceUID)
        cfind.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(SeriesLevel), dtags, maxNum)

    private def findImages(seriesUid: String, maxNum: Option[Int] = None)(using cfind: CFind) = 
        val dtags = DicomTags((Tag.SeriesInstanceUID, seriesUid), Tag.SOPInstanceUID)
        cfind.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(ImageLevel), dtags, maxNum)

    private lazy val dicomReader = ImageIO.getImageReadersByFormatName("DICOM").next()

    private def getDicomStream(sopInstanceUid: String)(using cget: CGet) = 
        cget.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), DicomTags((Tag.SOPInstanceUID, sopInstanceUid)))
            .map(_ => cget.getDicomInputStreamAndFree(sopInstanceUid))

    private def getTwoDicomStreams(sopInstanceUid: String)(using cget: CGet) = 
        cget.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), DicomTags((Tag.SOPInstanceUID, sopInstanceUid)))
            .map(_ => 
                cget.getDicomInputStream(sopInstanceUid).flatMap(d1 =>
                    cget.getDicomInputStreamAndFree(sopInstanceUid).map(d2 => (d1, d2))))

    import java.awt.image.BufferedImage
    import java.io.File
    import java.nio.file.FileSystems

    private val tess = new Tesseract()
    private val windows = "windows.*".r
    private val winTess = "C:\\Program Files\\Tesseract-OCR"
    private val mac = "mac.*".r
    private val macBrewTess = "/opt/homebrew/Cellar/tesseract/5.3.3"
    private val linux = "linux.*".r
    private def findTesseract = 
        System.getProperty("os.name").toLowerCase() match
            case windows() if os.exists(os.Path(winTess)) => Some(winTess)
            case mac() if os.exists(os.Path(macBrewTess)) => Some(macBrewTess)
            case _ => None
                
    private def saveAndOCR(i: Int, ds: Tuple2[DicomInputStream, DicomInputStream], tp: Option[String] = None) = 
        IO.blocking:
            // single instance of DicomInputStream works fine at CTDose.scala, but fails in here
            // provide separate instances for readDataset and read
            val attrs = ds._1.readDataset()
            dicomReader.setInput(ds._2)
            val im = dicomReader.read(0, dicomReader.getDefaultReadParam())
            val institution = Option(attrs.getString(Tag.InstitutionName))
            val fname = os.pwd / s"dose$i.png"
            javax.imageio.ImageIO.write(im, "png", fname.toIO)
            tp.map: p => 
                val osname = System.getProperty("os.name").toLowerCase()
                val (jnaPath, dataPath) = osname match 
                    case windows() => (os.Path(p), os.Path(p) / "tessdata")
                    case _ => (os.Path(p) / "lib", os.Path(p) / "share" / "tessdata")
                System.setProperty("jna.library.path", jnaPath.toString)
                tess.setDatapath(dataPath.toString)
                tess.setLanguage("eng")
                (tess.doOCR(im), institution)

    private val successfulPing = "PING was succussful.\nThe server is reachable and the server responded."
    private val unsuccessfulPing = "PING failed.\nServer is not reachable or the sever did not respond."
    private def printline = IO.println("-" * 80)

    private def runTestsAndSave(host: String, port: Int, called: String, calling: String, encoding: String) =
        DicomBase.resources(host, port, called, calling, encoding, false).use :
            case (cfind, cget) =>
                given CFind = cfind
                given CGet = cget
                cfind.ping.flatMap(_ match
                    case true =>
                        IO.println(successfulPing) *> printline *>
                        { for 
                            cts      <- todaysCTs
                            ctsorder =  cts.sortBy(_.apply(2).value)
                            _        <- ctsorder.traverse(tags => IO.println(s"${tags(2).value} : (${tags(3).value}) [${tags(4).value}]"))
                                        *> printline *> IO.println(s"Found ${cts.length} CT studies") 
                            lses     <- findSeries(ctsorder.last.last.value)
                            _        <- printline *> IO.println(s"Last CT study of Patient ${ctsorder.last.apply(3).value}(${ctsorder.last.apply(4).value}) has ${lses.length} series")
                                        *> lses.zipWithIndex.traverse :
                                            case (tags, i) => IO.println(s"${i+1}) : ${tags(1).value}(${tags(2).value})")
                            choice   <- printline *> printAndRead(s"Which one is dose report series? (1-${lses.length})", Some(numberInRange(1 to lses.length)))
                            lis      <- findImages(lses(choice.toInt - 1).last.value)
                            _        <- printline *> IO.println(s"Dose report series has ${lis.length} images")
                            edcms    <- lis.traverse(is => getTwoDicomStreams(is.last.value))
                            (e, ds)  =  edcms.partition(_.isLeft)
                            errs     =  e.flatMap(_.swap.toOption)
                            dcms     =  ds.flatMap(_.toOption)
                            tpath    <- printAndRead(s"Got ${dcms.length} images with ${errs.length} error(s). Enter the path where tesseract is installed.", default = findTesseract)
                            otpath   =  if tpath.isBlank() then None else Some(tpath.trim())
                            ocrs     <- dcms.zipWithIndex.traverse :
                                            case (dcm, i) => saveAndOCR(i, dcm, otpath).flatMap({
                                                    _ match
                                                        case Some((ans, oinst)) => 
                                                            printline *> IO.println("Result") *> IO.println(ans) *> printline
                                                            *> IO.pure(oinst)
                                                        case _ => 
                                                            IO.pure(None)
                                                })
                            files    =  if ocrs.length == 1 then 
                                            "1 file. (dose0.png)" 
                                        else 
                                            s"${ocrs.length} files. (dose0.png - dose${ocrs.length-1}.png)"
                            insts    =  ocrs.flatten.distinct.sorted
                            _        <- printline *> IO.println(s"Stored $files")
                            _        <- otpath match 
                                            case Some(tp) => 
                                                IO(os.write.over(os.pwd / "yader.conf", config(host, port, called, calling, encoding, tp, insts)))
                                                *> printline *> IO.println("Saved yader.conf file.")
                                            case _ => IO.unit
                        yield () }
                    case false =>
                        IO.println(unsuccessfulPing)
                )
    
    def run(as: List[String]): IO[ExitCode] = 
        (for 
            _           <-  printHello
            sinfo       <-  getServer
            _           <-  runTestsAndSave.tupled(sinfo)                 
        yield ())
        *> IO(ExitCode.Success)

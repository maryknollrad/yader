import net.maryknollrad.d4cs.{CFind, CGet, DicomBase, RetrieveLevel, DicomTags}
import RetrieveLevel.*
import DicomBase.StringTag
import cats.effect.kernel.Resource
import cats.effect.IO
import java.time.LocalDate
import org.dcm4che3.data.Tag
import java.awt.image.BufferedImage
import scala.util.chaining.*
import org.dcm4che3.imageio.plugins.dcm.*   
import org.dcm4che3.io.DicomInputStream
import javax.imageio.ImageIO

object CTDose:
    case class Dose(ctvol: Double, dlp: Double)
    type ExtractionError = String
    type DoseExtractor = String => Either[ExtractionError, Dose] // studyInstanceUID => dose or error
    type FindResource = Resource[IO, CFind]
    type GetResource = Resource[IO, CGet]

    def findResource(callingAe: String, calledAe: String, remoteHost: String, remotePort: Int, encoding: String = "euc-kr") = 
        Resource.make
            (IO(CFind(callingAe, calledAe, remoteHost, remotePort, encoding)))
            (f => IO(f.shutdown()))

    def getResource(callingAe: String, calledAe: String, remoteHost: String, remotePort: Int) = 
        Resource.make
            (IO(CGet(callingAe, calledAe, remoteHost, remotePort, false)))
            (g => IO(g.shutdown()))

    def findCTStudies(find: CFind, d: LocalDate = LocalDate.now()): IO[Seq[Seq[StringTag]]] = 
        import DicomBase.LocalDate2String
        val dtag = DicomTags((Tag.ModalitiesInStudy, "CT"), (Tag.StudyDate, d:String),
            Tag.AccessionNumber, Tag.PatientID, Tag.PatientName, Tag.PatientSex, 
            Tag.StudyDescription, Tag.StudyInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(StudyLevel), dtag)

    def findSeries(find: CFind, studyUid: String): IO[Seq[Seq[StringTag]]] = 
        val dtag = DicomTags((Tag.StudyInstanceUID, studyUid), Tag.AccessionNumber, Tag.SeriesNumber, Tag.SeriesDescription, Tag.SeriesInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(SeriesLevel), dtag)

    def findImages(find: CFind, seriesUid: String): IO[Seq[Seq[StringTag]]] = 
        val dtag = DicomTags((Tag.SeriesInstanceUID, seriesUid), Tag.AccessionNumber, Tag.SOPInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(ImageLevel), dtag)

    private val dicomReader = ImageIO.getImageReadersByFormatName("DICOM").next()
    def getImage(get: CGet, sopInstanceUid: String): IO[BufferedImage] = 
        for 
            _ <- get.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), DicomTags((Tag.SOPInstanceUID, sopInstanceUid)))
            im <- IO:
                    val storage = get.getImageStorage()
                    val dis = DicomInputStream(java.io.ByteArrayInputStream(storage.apply(sopInstanceUid.trim).toByteArray()))
                    dicomReader.setInput(dis)
                    dicomReader.read(0, dicomReader.getDefaultReadParam())
        yield im

    private def seriesBySeriesNumber(sesTags: Seq[Seq[StringTag]], targetNumber: Int) = 
        val targetString = targetNumber.toString()
        sesTags.find(seTags => seTags.exists(t => t.tag == Tag.SeriesNumber && t.value == targetString))
            .toRight(s"Cannot find series number $targetNumber (Accession Number : ${sesTags.head.pipe(accessionNumber)})")

    private def seriesByIndex(sesTags: Seq[Seq[StringTag]], index: Int) = 
        val seqSize = sesTags.size
        val i = if index >= 0 then index min (seqSize - 1) else seqSize - (-index min sesTags.size)
        sesTags(i)

    private def accessionNumber(tags: Seq[StringTag]) = 
        tags.find(_.tag == Tag.AccessionNumber).toRight("Can't find Accession Number tag")

    private def drawString(bi: BufferedImage, s: String) = 
        val gh = bi.getGraphics()
        gh.drawString(s, 20, 20)
        bi

    case class ConnectionInfo(callingAe: String, calledAe: String, host: String, port: Int, encoding: String) 

    def abc(ci: ConnectionInfo) = 
        import cats.syntax.all.*

        val r = for 
            cfind <- findResource(ci.callingAe, ci.calledAe, ci.host, ci.port, ci.encoding)
            cget <- getResource(ci.callingAe, ci.calledAe, ci.host, ci.port)
        yield (cfind, cget)

        r.use:
            case (cfind, cget) =>
                for             
                    // CTs Tags : Seq[Seq[StringTag]]
                    ctsTags <- findCTStudies(cfind)
                    // find seriesInstanceUIDs using studyInstanceUID
                    // EitherSeriesesTags : Seq[Either[String, Seq[Seq[StringTag]]]] - collection of StringTag (Seq[StringTag]) per series (Seq[Seq[StringTag]])
                    eSesTagss <- ctsTags.traverse(tags =>  
                                val etag = tags.find(_.tag == Tag.StudyInstanceUID).toRight(s"Can't find StudyInstanceUID Tag (Accession Number : ${accessionNumber(tags)})")
                                etag.map(t => findSeries(cfind, t.value)).sequence)
                    // find SOPInstanceUIDs using seriesInstanceUID
                    eImsTagss <- eSesTagss.traverse(eSesTags =>
                                val eSeTag = eSesTags.flatMap(seriesBySeriesNumber(_, 9000))
                                eSeTag.flatMap(ts => 
                                    ts.find(_.tag == Tag.SeriesInstanceUID)
                                        .toRight(s"Can't find SeriesInstanceUID Tag (AccessionNumber : ${accessionNumber(ts)})"))
                                .map(t => findImages(cfind, t.value)).sequence)
                    // convert each dose report series SOPInstanceUIDs => BufferedImages
                    eBIs <- eImsTagss.traverse(eImsTags =>
                                    val eImUIDANs: Either[String, Seq[(String, String)]] = eImsTags.flatMap(imsTags =>
                                        if (imsTags.length == 0) then Left("DoseReport Series does not contains image")
                                        else
                                            val imUids = imsTags.map: imTags => 
                                                // filter SOPInstanceUID & AccessionNumber Tags
                                                val (oacno, ouid) = imTags.foldLeft(Option.empty[String], Option.empty[String]) : 
                                                    case ((oacno, ouid), t) =>
                                                        if t.tag == Tag.SOPInstanceUID then (oacno, Some(t.value))
                                                        else if t.tag == Tag.AccessionNumber then (Some(t.value), ouid)
                                                        else (oacno, ouid)
                                                (oacno, ouid) match
                                                    case (Some(acno), Some(uid)) =>
                                                        Right((acno, uid))
                                                    case _ =>
                                                        Left(s"Can't find SOPInstanceUID or AccessionNumber (AccessionNumber : ${accessionNumber(imTags)})")
                                            // if any image is invalid than all is invalid
                                            imUids.find(_.isLeft) match 
                                                case Some(Left(l)) => Left(l)
                                                case _ => Right(imUids.flatMap(_.toOption)))
                                    // get images using SOPInstanceUID and mark AccessionNumber
                                    eImUIDANs.map(imUIDANs =>
                                        imUIDANs.traverse((acno, uid) => 
                                            getImage(cget, uid).map(bi => drawString(bi, acno)))).sequence)
                    (errs, bis) = eBIs.partition(_.isLeft)
                yield (errs.flatMap(_.swap.toOption), bis.flatMap(_.toOption))  // flatten LEFTs and RIGHTs
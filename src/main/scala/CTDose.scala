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

    private val reader = ImageIO.getImageReadersByFormatName("DICOM").next()
    def getImage(get: CGet, sopInstanceUid: String): IO[BufferedImage] = 
        for 
            _ <- get.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), DicomTags((Tag.SOPInstanceUID, sopInstanceUid)))
            im <- IO:
                    val storage = get.getImageStorage()
                    val dis = DicomInputStream(java.io.ByteArrayInputStream(storage.apply(sopInstanceUid.trim).toByteArray()))
                    reader.setInput(dis)
                    reader.read(0, reader.getDefaultReadParam())
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
        // given fr: FindResource = findResource("READROOM", "NETGEAR_EXTERNAL", "192.168.10.133", 105, "euc_kr")
        // given gr: GetResource = getResource("READROOM", "NETGEAR_EXTERNAL", "192.168.10.133", 105)

        val r = for 
            find <- findResource(ci.callingAe, ci.calledAe, ci.host, ci.port, ci.encoding)
            get <- getResource(ci.callingAe, ci.calledAe, ci.host, ci.port)
        yield (find, get)

        r.use:
            case (find, get) =>
                for             
                    // CTs Tags : Seq[Seq[StringTag]]
                    ctsTags <- findCTStudies(find)
                    // EitherSeriesesTags : Seq[Either[String, Seq[Seq[StringTag]]]] - collection of StringTag (Seq[StringTag]) per series (Seq[Seq[StringTag]])
                    eSesTagss <- ctsTags.traverse(tags =>  
                                val etag = tags.find(_.tag == Tag.StudyInstanceUID) match 
                                    case Some(tag) =>
                                        Either.right[ExtractionError, StringTag](tag)
                                    case None =>
                                        Either.left[ExtractionError, StringTag](s"Can't find StudyInstanceUID tag of CT (Accession Number : ${accessionNumber(tags)})")
                                etag.map(t => findSeries(find, t.value)).sequence)
                    // 
                    eImsTagss <- eSesTagss.traverse(eSesTags =>
                                val eSeTag = eSesTags.flatMap(seriesBySeriesNumber(_, 9000))
                                eSeTag.flatMap(ts => 
                                    ts.find(_.tag == Tag.SeriesInstanceUID)
                                        .toRight(s"Can't find SeriesInstanceUID (AccessionNumber : ${accessionNumber(ts)})"))
                                .map(t => findImages(find, t.value)).sequence)
                    eBIs <- eImsTagss.traverse(eImsTags =>
                                    val eImUIDAN = eImsTags.flatMap(imsTags =>
                                        imsTags match 
                                            case Seq(imTags) => // one
                                                val (ouid, oacno) = imTags.foldLeft(Option.empty[String], Option.empty[String]) : 
                                                    case ((uid, acno), t) =>
                                                        if t.tag == Tag.SOPInstanceUID then (Some(t.value), acno)
                                                        else if t.tag == Tag.AccessionNumber then (uid, Some(t.value))
                                                        else (uid, acno)
                                                (ouid, oacno) match
                                                    case (Some(uid), Some(acno)) =>
                                                        Right((uid, acno))
                                                    case _ =>
                                                        Left(s"Can't find SOPInstanceUID or AccessionNumber (AccessionNumber : ${accessionNumber(imTags)})")
                                            case _ =>
                                                Left(s"DoseReport Series image count is not one ${imsTags.length} (Accession Number : ${imsTags.headOption.map(accessionNumber)})"))
                                    // eImUIDAN.map((uid,acno) => getImage(get, uid).map(bi => (bi, acno))).sequence)
                                    eImUIDAN.map((uid,acno) => getImage(get, uid).map(bi => drawString(bi, acno))).sequence)
                    (errs, bis) = eBIs.partition(_.isLeft)
                yield (errs.flatMap(_.swap.toOption), bis.flatMap(_.toOption))
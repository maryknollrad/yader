package net.maryknollrad.ctdose

import net.maryknollrad.d4cs.{CFind, CGet, DicomBase, RetrieveLevel, DicomTags}
import RetrieveLevel.*
import DicomBase.StringTag
import cats.effect.kernel.Resource
import cats.effect.IO
import java.time.LocalDate
import org.dcm4che3.data.{Tag, ElementDictionary}
import java.awt.image.BufferedImage
import scala.util.chaining.*
import org.dcm4che3.imageio.plugins.dcm.*   
import org.dcm4che3.io.DicomInputStream
import javax.imageio.ImageIO
import scala.util.matching.Regex.Match

object CTDose:
    case class DoseResultRaw(acno: String, ocrResult: String, image: BufferedImage)
    type ExtractionError = String
    // type DoseExtractor = String => Either[ExtractionError, Dose] // studyInstanceUID => dose or error
    type FindResource = Resource[IO, CFind]
    type GetResource = Resource[IO, CGet]

    def findResource(ci: Configuration.ConnectionInfo) = 
        Resource.make
            (IO(CFind(ci.callingAe, ci.calledAe, ci.host, ci.port, ci.encoding)))
            (f => IO(f.shutdown()))

    def getResource(ci: Configuration.ConnectionInfo) = 
        Resource.make
            (IO(CGet(ci.callingAe, ci.calledAe, ci.host, ci.port, false)))
            (g => IO(g.shutdown()))

    def gather2Map(tags: Seq[StringTag], level: String, beginMap: Map[String, String] = Map.empty) = 
        tags.foldLeft(beginMap)((map, t) => 
            val tagString = ElementDictionary.keywordOf(t.tag, null)
            map.updated(s"${level.toUpperCase()}_${tagString}", t.value))

    def findCTStudies(find: CFind, d: LocalDate): IO[Seq[Seq[StringTag]]] = 
        import DicomBase.LocalDate2String
        val dtag = DicomTags((Tag.ModalitiesInStudy, "CT"), (Tag.StudyDate, d:String), Tag.StudyTime, 
            Tag.AccessionNumber, Tag.PatientID, Tag.PatientName, Tag.PatientSex, Tag.PatientBirthDate,
            Tag.Manufacturer, Tag.ManufacturerModelName, Tag.CTDIvol, 
            Tag.StudyDescription, Tag.StudyInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(StudyLevel), dtag)

    def findSeries(find: CFind, studyUid: String): IO[Seq[Seq[StringTag]]] = 
        val dtag = DicomTags((Tag.StudyInstanceUID, studyUid), 
            Tag.AccessionNumber, Tag.SeriesNumber, Tag.SeriesDescription, Tag.CTDIvol, Tag.SeriesInstanceUID)

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

    def findDoubleStringInMatch(m: Match) = 
        m.subgroups.flatMap(s => scala.util.Try(s.toDouble).toOption).headOption.getOrElse(-1.0)

    def getCTDoses(ci: Configuration.ConnectionInfo, d: LocalDate = LocalDate.now()) = 
        import cats.syntax.all.*

        val r = for 
            cfind <- findResource(ci)
            cget <- getResource(ci)
        yield (cfind, cget)

        r.use:
            case (cfind, cget) =>
                for             
                    // CTs Tags : Seq[Seq[StringTag]]
                    ctsTags <- findCTStudies(cfind, d).map(_.take(2))
                    // find seriesInstanceUIDs using studyInstanceUID
                    // EitherSeriesesTags : Seq[Either[String, Seq[Seq[StringTag]]]] - collection of StringTag (Seq[StringTag]) per series (Seq[Seq[StringTag]])
                    eSesTagssWithMap <- ctsTags.traverse(ctTags =>
                                val studyMap = gather2Map(ctTags, "STUDY")
                                // println(s"Got : $ctTags => Converted : $studyMap")
                                val etag = ctTags.find(_.tag == Tag.StudyInstanceUID).toRight(s"Can't find StudyInstanceUID Tag (Accession Number : ${accessionNumber(ctTags)})")
                                etag.map(t => findSeries(cfind, t.value).map((_, studyMap))).sequence)
                    // find SOPInstanceUIDs using seriesInstanceUID
                    eImsTagssWithMap <- eSesTagssWithMap.traverse(eSesTagsWithMap =>
                                val eSeriesMap = eSesTagsWithMap.map((seriesTags, studyMap) => 
                                                    seriesTags.zipWithIndex.foldLeft(studyMap) :
                                                        case (mergingMap, (stags, i)) => gather2Map(stags, s"S$i", mergingMap))
                                // TODO: dose series selection should be configurable
                                val eDoseSeriesTag = eSesTagsWithMap.flatMap((stags, _) => seriesBySeriesNumber(stags, 9000))
                                val eDoseSeriesWithMap = eSeriesMap.flatMap(map => eDoseSeriesTag.map((_, map)))
                                eDoseSeriesWithMap.flatMap((ts, map) => 
                                    ts.find(_.tag == Tag.SeriesInstanceUID)
                                        .toRight(s"Can't find SeriesInstanceUID Tag (AccessionNumber : ${accessionNumber(ts)})").map((_, map)))
                                .map((t, map) => findImages(cfind, t.value).map((_, map))).sequence)
                    // convert each dose report series SOPInstanceUIDs => BufferedImages
                    eBIsWithMap <- eImsTagssWithMap.traverse(eImsTags =>
                                    val eImUIDANsWithMap: Either[String, (Seq[(String, String)],Map[String, String])] = eImsTags.flatMap((imsTags, map) =>
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
                                                case _ => Right(imUids.flatMap(_.toOption)).map((_, map)))
                                    // get images using SOPInstanceUID and mark AccessionNumber
                                    eImUIDANsWithMap.traverse({ case (imUIDANs, map) =>
                                        imUIDANs.traverse((acno, uid) => 
                                            getImage(cget, uid).map(bi => 
                                                val ocrResult = Tesseract.doOCR(bi)
                                                val marked = drawString(bi, acno)
                                                DoseResultRaw(acno, ocrResult, marked)
                                                )).map((_, map))}))
                    (errs, bis) = eBIsWithMap.partition(_.isLeft)
                yield (errs.flatMap(_.swap.toOption), bis.flatMap(_.toOption))  // flatten LEFTs and RIGHTs
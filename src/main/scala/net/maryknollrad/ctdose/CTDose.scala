package net.maryknollrad.ctdose

import net.maryknollrad.d4cs.{CFind, CGet, DicomBase, RetrieveLevel, DicomTags}
import RetrieveLevel.*
import DicomBase.*
import cats.effect.kernel.Resource
import cats.effect.IO
import cats.data.EitherT
import java.time.LocalDate
import org.dcm4che3.data.{Tag, ElementDictionary, Attributes}
import java.awt.image.BufferedImage
import scala.util.chaining.*
import org.dcm4che3.imageio.plugins.dcm.*   
import org.dcm4che3.io.DicomInputStream
import javax.imageio.ImageIO
import scala.util.matching.Regex.Match
import cats.syntax.all.*
// import Demo.dose

object CTDose:
    case class DoseResultRaw(acno: String, ocrResult: String, image: BufferedImage)
    type ExtractionError = String
    // type DoseExtractor = String => Either[ExtractionError, Dose] // studyInstanceUID => dose or error
    type FindResource = Resource[IO, CFind]
    type GetResource = Resource[IO, CGet]
    type SOPInstanceUIDExtractor[A] = (CGet, String) => IO[A]

    def findResource(ci: Configuration.ConnectionInfo) = 
        Resource.make
            (IO.blocking(CFind(ci.callingAe, ci.calledAe, ci.host, ci.port, ci.encoding)))
            (f => IO.blocking(f.shutdown()))

    def getResource(ci: Configuration.ConnectionInfo) = 
        Resource.make
            (IO.blocking(CGet(ci.callingAe, ci.calledAe, ci.host, ci.port, false)))
            (g => IO.blocking(g.shutdown()))

    private def gather2Map(tags: Seq[StringTag], level: String, beginMap: Map[String, String] = Map.empty) = 
        tags.foldLeft(beginMap)((map, t) => 
            val tagString = ElementDictionary.keywordOf(t.tag, null)
            map.updated(s"${level.toUpperCase()}_${tagString}", t.value))

    def findCTStudies(find: CFind, d: LocalDate, maxNum: Option[Int] = None): IO[Seq[Seq[StringTag]]] = 
        import DicomBase.LocalDate2String
        val dtag = DicomTags((Tag.ModalitiesInStudy, "CT"), (Tag.StudyDate, d:String),
            Tag.AccessionNumber, Tag.StudyInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(StudyLevel), dtag, maxNum)

    def findSeries(find: CFind, studyUid: String, maxNum: Option[Int] = None): IO[Seq[Seq[StringTag]]] = 
        val dtag = DicomTags((Tag.StudyInstanceUID, studyUid), 
            Tag.AccessionNumber, Tag.SeriesDescription, Tag.SeriesNumber, Tag.SeriesInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(SeriesLevel), dtag, maxNum)

    def findImages(find: CFind, seriesUid: String, maxNum: Option[Int] = None): IO[Seq[Seq[StringTag]]] = 
        val dtag = DicomTags((Tag.SeriesInstanceUID, seriesUid), 
            Tag.AccessionNumber, Tag.SOPInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(ImageLevel), dtag, maxNum) // .pipe(ioprint("WITHIN FIND_IMAGES"))

    private val dicomReader = ImageIO.getImageReadersByFormatName("DICOM").next()

    private def getDicomStream(get: CGet, sopInstanceUid: String): IO[DicomInputStream] = 
        for 
            _ <- get.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), DicomTags((Tag.SOPInstanceUID, sopInstanceUid)))
        yield
            val storage = get.getImageStorage()
            DicomInputStream(java.io.ByteArrayInputStream(storage.apply(sopInstanceUid.trim).toByteArray()))

    def getAttributes(get: CGet, sopInstanceUid: String): IO[Attributes] = 
        getDicomStream(get, sopInstanceUid).map(_.readDataset) // .pipe(ioprint("GET ATTRIBUTE"))

    def getImageWithAttributes(get: CGet, sopInstanceUid: String): IO[(BufferedImage, Attributes)] = 
        getDicomStream(get, sopInstanceUid).map: dis =>
            dicomReader.setInput(dis)
            val attrs = dis.readDataset()
            (dicomReader.read(0, dicomReader.getDefaultReadParam()), attrs)

    private def seriesBySeriesNumber(sesTags: Seq[Seq[StringTag]], targetNumber: Int) = 
        val targetString = targetNumber.toString()
        sesTags.find(seTags => seTags.exists(t => t.tag == Tag.SeriesNumber && t.value == targetString))
            .toRight(s"Cannot find series number $targetNumber ${sesTags.head.pipe(accessionNumber)}")

    private def seriesByIndex(sesTags: Seq[Seq[StringTag]], index: Int) = 
        val seqSize = sesTags.size
        val i = if index >= 0 then index min (seqSize - 1) else seqSize - (-index min sesTags.size)
        sesTags(i)

    private def seriesBySeriesName(sesTags: Seq[TagValues], targetString: String) = 
        sesTags.find(seTags => seTags.exists(t => t.tag == Tag.SeriesDescription && t.value.contains(targetString)))
            .toRight(s"""Cannot find series containing "$targetString" (Accession Number : ${sesTags.head.pipe(accessionNumber)})""")

    private def accessionNumber(tags: Seq[StringTag]) = 
        tags.find(_.tag == Tag.AccessionNumber).map(t => s"(Accession Number) : ${t.value}").getOrElse("Can't find Accession Number tag")

    private def accessionNumberOfInfo(info: Seq[(Int, String)]) = 
        info.find(_._1 == Tag.AccessionNumber).map(t => s"(Accession Number) : ${t._2}").getOrElse("Can't find Accession Number tag")

    private def drawString(bi: BufferedImage, s: String) = 
        val gh = bi.getGraphics()
        gh.drawString(s, 20, 20)
        bi

    def findDoubleStringInMatch(m: Match) = 
        m.subgroups.flatMap(s => scala.util.Try(s.toDouble).toOption).headOption.getOrElse(-1.0)

    private def gatherSeries(ctTags: TagValues, maxNum: Option[Int] = None)(using cfind: CFind) : IO[Either[String, (TagValue, Seq[TagValues])]] = 
        ctTags
            .find(_.tag == Tag.StudyInstanceUID).toRight(s"Can't find StudyInstanceUID Tag ${accessionNumber(ctTags)}")
            .map(t => 
                findSeries(cfind, t.value, maxNum).map(seTags => (t, seTags))
            ).sequence

    private def gatherImages(eCTSeTags: Either[String, (TagValue, TagValues)], maxNum: Option[Int] = None)(using cfind: CFind): 
                                IO[Either[String, (StringTag, StringTag, Seq[Seq[StringTag]])]] = 
        eCTSeTags.flatMap((ctUid, seTags) =>
            // collapsed either of seriesInstanceUIDs
            val etags = seTags.find(_.tag == Tag.SeriesInstanceUID).toRight(s"Can't find SeriesInstanceUID Tag ${accessionNumber(seTags)}")
            etags.map(seuid => 
                findImages(cfind, seuid.value, maxNum).map(itags => (ctUid, seuid, itags)) //.pipe(ioprint("WITHIN GATHER_IMAGE"))
            )).sequence

    private def toSOPInstanceUIDs(eCTSeImTagss: Either[String, Seq[(StringTag, StringTag, Seq[Seq[StringTag]])]]) =
        eCTSeImTagss.flatMap(ctSeImTagss =>
            ctSeImTagss.flatMap((stUid, seUid, imTagss) => 
                imTagss.map(imTags =>
                    imTags.find(_.tag == Tag.SOPInstanceUID).toRight(s"Can't find SeriesInstanceUID Tag ${accessionNumber(imTags)}")
                        .map((stUid, seUid, _))
                )
            ).sequence
        )

    val defaultCollectTags = Seq(Tag.AccessionNumber, Tag.PatientID, Tag.PatientSex, Tag.PatientBirthDate, 
        Tag.StudyDescription, Tag.ProtocolName, Tag.StudyDate, Tag.StudyTime, 
        Tag.BodyPartExamined, Tag.Manufacturer, Tag.ManufacturerModelName, Tag.StationName, Tag.OperatorsName)

    private def ioprint[A](msg: String = "")(ans: IO[A]) = ans.flatMap(a => IO({println(s"$msg : $a"); a}))

    def getCTDoses(ci: Configuration.ConnectionInfo, d: LocalDate = LocalDate.now(), collectTags: Seq[Int] = defaultCollectTags, encoding: String = "utf-8")
            :IO[(Seq[(Seq[(Int, String)], Seq[DoseResultRaw])], Seq[String])] = 
        val r = for 
            cfind <- findResource(ci)
            cget <- getResource(ci)
        yield (cfind, cget)

        r.use:
            case (cfind, cget) =>
                given CFind = cfind
                given CGet = cget
                val startTime = System.nanoTime()
                for
                    // CTs Tags : Seq[Seq[StringTag]]
                    ctTagss             <-  findCTStudies(cfind, d)
                    /* POSSIBLY IMPORTANT
                       map(...).sequence is easier to read because metal informs current type
                       but possibly map evaluated simultaneosly, causing strange results esp. dealing with external library
                    */
                    seTagss             <-  ctTagss.traverse(ctTags => gatherSeries(ctTags))
                    ct1SeImgUidsss      <-  seTagss.traverse(eSeTagss => 
                                                eSeTagss.map{ case (stuid, seTags) => 
                                                    val firstSeriesTags = 
                                                        seTags.headOption.toRight(s"Study contains no series (StudyInstanceUID: $stuid)")
                                                            .map(t => (stuid, t))
                                                    gatherImages(firstSeriesTags, Some(1))
                                                        .map(eImgTags =>
                                                            eImgTags.flatMap((_, seuid, itags) => 
                                                                itags.headOption.toRight(s"Series contains no images (SeriesInstanceUID: $seuid)")
                                                                    .flatMap(_.find(_.tag == Tag.SOPInstanceUID).toRight(s"First Image contains no SOPInstanceUID tag (SeriesInstanceUID : $seuid})"))
                                                        ))
                                                }.flatSequence  
                                                /*  MAJOR CHANGES from sequence to flatSequence 
                                                    Either[IO[Either[(StringTag, StringTag, Seq[Seq[StringTag]])]]]
                                                    => IO[Either[(StringTag, StringTag, Seq[Seq[StringTag]])]] */
                                            )
                    eattrs              <-  ct1SeImgUidsss.traverse(eImgUid => eImgUid.traverse(tag => 
                                                getAttributes(cget, tag.value)))
                    ctSeUidsWithInfos   =   seTagss.zip(eattrs).map:
                                                case (eSeTagss, eAttr) =>
                                                    assert(seTagss.length == eattrs.length)
                                                    eSeTagss.flatMap(seTagss => 
                                                        eAttr.flatMap(attr => 
                                                            val (tagValues, oman, omod) = collectTags.foldLeft((Seq.empty[(Int, String)], Option.empty[String], Option.empty[String])):
                                                                case ((collect, oman, omod), tag) =>
                                                                    val svalue = Option(attr.getBytes(tag))
                                                                            .map(s => String(s, encoding).trim)
                                                                            .getOrElse(s"*Empty ${ElementDictionary.keywordOf(tag, null)}")
                                                                    val ncollect = collect :+ (tag, svalue)
                                                                    tag match 
                                                                        case Tag.Manufacturer =>
                                                                            (ncollect, Some(svalue), omod)
                                                                        case Tag.ManufacturerModelName =>
                                                                            (ncollect, oman, Some(svalue))
                                                                        case _ =>
                                                                            (ncollect, oman, omod)
                                                            oman.flatMap(man => omod.map(mod => (seTagss._2, tagValues, man, mod))).toRight(s"Cannot find Manufacturer or Model in (StudyInstanceUID : ${seTagss._1})")
                                                        ))                                        
                    doseUidsWithInfo    <-  ctSeUidsWithInfos.traverse(eCTSeUids =>
                                                eCTSeUids.map({ case (seTags, info, manufacturer, model) =>
                                                    val eDoseInfoSeries = seriesBySeriesNumber(seTags, 9000)
                                                    val eDiSeUid = eDoseInfoSeries.flatMap(dis => 
                                                        dis.find(_.tag == Tag.SeriesInstanceUID).toRight(s"Cannot find SeriesInstanceUID of dose info series ${accessionNumber(dis)}"))
                                                    eDiSeUid.map(diSeUid => 
                                                        findImages(cfind, diSeUid.value).map(itagss => 
                                                            val eiuids = itagss.traverse(tags =>
                                                                tags.find(_.tag == Tag.SOPInstanceUID).toRight(s"Cannot find SOPInstanceUID ${accessionNumberOfInfo(info)}"))
                                                            eiuids.map((info, _)))
                                                    ).flatSequence
                                                }).flatSequence
                                            )
                    doseImagesWithInfo  <-  doseUidsWithInfo.traverse(eDoseUidWithInfo =>
                                                eDoseUidWithInfo.traverse((info, imgUids) =>
                                                    imgUids.traverse(imgUid => 
                                                        val acno = info.find(_._1 == Tag.AccessionNumber).map(_._2).getOrElse(s"No Accession Number ${imgUid.value}")
                                                        getDicomStream(cget, imgUid.value).map(ds =>
                                                            dicomReader.setInput(ds)
                                                            val im = dicomReader.read(0, dicomReader.getDefaultReadParam())
                                                            val ocrResult = Tesseract.doOCR(im)
                                                            val marked = drawString(im, acno)
                                                            DoseResultRaw(acno, ocrResult, marked)
                                                        )
                                                    ).map((info, _))
                                                )
                                            )
                    (errs, bis)         = doseImagesWithInfo.partition(_.isLeft)
                yield (bis.flatMap(_.toOption), errs.flatMap(_.swap.toOption))  // flatten RIGHTs and LEFTs
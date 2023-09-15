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
import net.maryknollrad.ctdose.CTDoseInfo.CTDoseResult
// import Demo.dose

object CTDose:
    case class DoseResultRaw(acno: String, ocrResult: Option[Double], ocrRaw: String, image: BufferedImage)
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
        val dtags = DicomTags((Tag.ModalitiesInStudy, "CT"), (Tag.StudyDate, d:String),
            Tag.AccessionNumber, Tag.StudyInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(StudyLevel), dtags, maxNum)

    def findSeries(find: CFind, studyUid: String, maxNum: Option[Int] = None): IO[Seq[Seq[StringTag]]] = 
        val dtags = DicomTags((Tag.StudyInstanceUID, studyUid), 
            Tag.AccessionNumber, Tag.SeriesDescription, Tag.SeriesNumber, Tag.SeriesInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(SeriesLevel), dtags, maxNum)

    def findImages(find: CFind, seriesUid: String, maxNum: Option[Int] = None): IO[Seq[Seq[StringTag]]] = 
        val dtags = DicomTags((Tag.SeriesInstanceUID, seriesUid), 
            Tag.AccessionNumber, Tag.SOPInstanceUID)

        find.query(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelFind, Some(ImageLevel), dtags, maxNum)

    private val dicomReader = ImageIO.getImageReadersByFormatName("DICOM").next()

    private def getDicomStream(get: CGet, sopInstanceUid: String): IO[Either[String, DicomInputStream]] = 
        for 
            _ <- get.getStudy(org.dcm4che3.data.UID.StudyRootQueryRetrieveInformationModelGet, Some(ImageLevel), DicomTags((Tag.SOPInstanceUID, sopInstanceUid)))
        yield
            get.getDicomInputStreamAndFree(sopInstanceUid)

    def getAttributes(get: CGet, sopInstanceUid: String): IO[Either[String, Attributes]] = 
        getDicomStream(get, sopInstanceUid).map(edis => edis.map(_.readDataset)) // .pipe(ioprint("GET ATTRIBUTE"))

    def getImageWithAttributes(get: CGet, sopInstanceUid: String, imageIndex: Int = 0): IO[Either[String, (BufferedImage, Attributes)]] = 
        getDicomStream(get, sopInstanceUid).map: edis =>
            edis.map(dis => 
                dicomReader.setInput(dis)
                val attrs = dis.readDataset()
                (dicomReader.read(imageIndex, dicomReader.getDefaultReadParam()), attrs)
            )

    private def seriesBySeriesNumber(sesTags: Seq[Seq[StringTag]], targetNumber: Int) = 
        val targetString = targetNumber.toString()
        sesTags.find(seTags => seTags.exists(t => t.tag == Tag.SeriesNumber && t.value == targetString))
            .toRight(s"Cannot find series number $targetNumber ${sesTags.head.pipe(accessionNumber)}")

    private def seriesByIndex(sesTags: Seq[Seq[StringTag]], index: Int) = 
        val seqSize = sesTags.size
        val i = if index >= 0 then index min (seqSize - 1) else seqSize - (-index min seqSize)
        sesTags(i).asRight[String]

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

    def findFirstDoubleStringInMatch(m: Match) = 
        m.subgroups.flatMap(s => scala.util.Try(s.toDouble).toOption).headOption

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
                    imTags.find(_.tag == Tag.SOPInstanceUID).toRight(s"Can't find SOPInstanceUID Tag ${accessionNumber(imTags)}")
                        .map((stUid, seUid, _))
                )
            ).sequence
        )

    // should match study table fields, first 4 tags must be patient ID, sex, birthday, accession number
    // CONSIDER : request physicial, study order code, contrast material
    private val defaultCollectTags = Seq(Tag.PatientID, Tag.PatientSex, Tag.PatientBirthDate, Tag.AccessionNumber, 
        Tag.StudyDate, Tag.StudyTime, Tag.StudyDescription, Tag.ProtocolName, Tag.BodyPartExamined, 
        Tag.InstitutionName, Tag.Manufacturer, Tag.ManufacturerModelName, Tag.StationName, Tag.OperatorsName)
    def getDefaultCollectTags() = defaultCollectTags

    private def ioprint[A](msg: String = "")(ans: IO[A]) = ans.flatMap(a => IO({println(s"$msg : $a"); a}))

    /* POSSIBLY IMPORTANT
        map(...).sequence is easier to read than traverse because metal informs current type
        but possibly map evaluated simultaneosly, causing strange results esp. dealing with external library
    */
    private def series2firstSeriesFirstImageAttributes(seriesTags: Seq[Either[String, (TagValue, Seq[TagValues])]])
                    (using cfind: CFind, cget: CGet): IO[Seq[Either[String, Attributes]]] = 
        for 
            ct1SeImgUidsss      <-  seriesTags.traverse(eSeTagss => 
                                        eSeTagss.map({ case (stuid, seTags) => 
                                            val firstSeriesTags = 
                                                seTags.headOption.toRight(s"Study contains no series (StudyInstanceUID: $stuid) @ series2firstSeriesFirstImageAttributes")
                                                    .map(t => (stuid, t))
                                            gatherImages(firstSeriesTags, Some(1))
                                                .map(eImgTags =>    // firstSeries's images Tags with size 1
                                                    eImgTags.flatMap((_, seuid, itags) => 
                                                        itags.headOption.toRight(s"Series contains no images (SeriesInstanceUID: $seuid) @ series2firstSeriesFirstImageAttributes")
                                                            .flatMap(firstTags =>
                                                                firstTags.find(_.tag == Tag.SOPInstanceUID).toRight(s"First Image contains no SOPInstanceUID tag (SeriesInstanceUID : $seuid}) @ series2firstSeriesFirstImageAttributes"))))
                                        }).flatSequence
                                        /*  MAJOR CHANGES from sequence to flatSequence 
                                            Either[IO[Either[(StringTag, StringTag, Seq[Seq[StringTag]])]]]
                                            => IO[Either[(StringTag, StringTag, Seq[Seq[StringTag]])]] */
                                    )
            // Dicom Attributes of 1st image of 1st series per Study
            eattrs              <-  ct1SeImgUidsss.traverse(eImgUid => 
                                        eImgUid.flatTraverse(tag => 
                                            getAttributes(cget, tag.value)
                                    ))
        yield eattrs

    // (Series Tags (Seq[TagValues]), Study Attributes from above Attributes (Seq[(Int, String)]), Manufacturer, Model) per Study
    private def mergeSeriesWithAttributes(seriesTags: Seq[Either[String, (StringTag, Seq[Seq[StringTag]])]],
                attributes: Seq[Either[String, Attributes]], collectTags: Seq[Int], config: Configuration.CTDoseConfig) = 
        assert(seriesTags.length == attributes.length)
        seriesTags.zip(attributes).map({
            case (eSeTagss, eAttr) =>
                eSeTagss.flatMap(seTagss => 
                    eAttr.flatMap(attr => 
                        val (tagValues, oman, omod, attrWarns) = collectTags.foldLeft((Seq.empty[(Int, String)], Option.empty[String], Option.empty[String], Seq.empty[Int])):
                            case ((collect, oman, omod, attrWarns), tag) =>
                                val (svalue, nextWarn) = Option(attr.getBytes(tag))
                                        .map(s => (String(s, config.encoding).trim, attrWarns))
                                        .getOrElse({
                                            (s"*Empty ${ElementDictionary.keywordOf(tag, null)}", attrWarns :+ tag)
                                        })
                                val ncollect = collect :+ (tag, svalue)
                                tag match 
                                    case Tag.Manufacturer =>
                                        (ncollect, Some(svalue.toUpperCase()), omod, nextWarn)
                                    case Tag.ManufacturerModelName =>
                                        (ncollect, oman, Some(svalue.toUpperCase()), nextWarn)
                                    case _ =>
                                        (ncollect, oman, omod, nextWarn)
                        oman.flatMap(man => omod.map(mod => (seTagss._2, tagValues, man, mod, attrWarns))).toRight(s"Cannot find Manufacturer or Model in (StudyInstanceUID : ${seTagss._1}) @ mergeSeriesWithAttributes")
                    ))
            // filter out other hospital's exam
        }).filter(_ match
            case Right((_, info, _, _, _)) =>
                info.find((t, v) => t == Tag.InstitutionName && config.institution.contains(v.toUpperCase())).nonEmpty
            case _ => 
                false
        )

    // (Study Attributes, SOPInstanceUIDs of dose information series)
    private def selectDoseSeries(seriesTagsWithAttrs: Seq[Either[String, (Seq[Seq[StringTag]], Seq[(Int, String)], String, String, Seq[Int])]], 
                    diMap: CTDoseInfo.CTInfo)(using cfind: CFind) = 
        seriesTagsWithAttrs.traverse(eCTSeUids =>
            eCTSeUids.map({ case (seTags, info, manufacturer, model, attrWarns) =>
                val edi = diMap.get((manufacturer, model)).toRight(s"Cannot find information of $manufacturer.$model @ selectDoseSeries")
                val eDoseInfoSeries: Either[String, Seq[StringTag]] = 
                    edi.flatMap(di => 
                        (di.seriesInfo.by, di.seriesInfo.value) match
                            case (Some(CTDoseInfo.CTDoseSeries.SeriesNumber), Some(v: Int)) =>
                                seriesBySeriesNumber(seTags, v)
                            case (Some(CTDoseInfo.CTDoseSeries.SeriesIndex), Some(v: Int)) =>
                                seriesByIndex(seTags, v)
                            case (Some(CTDoseInfo.CTDoseSeries.SeriesName), Some(v: String)) =>
                                seriesBySeriesName(seTags, v)
                            case _ =>
                                s"Wrong value or value does not exist in ... @ selectDoseSeries".asLeft[Seq[StringTag]]
                    )
                val eDiSeUid = eDoseInfoSeries.flatMap(dis => 
                    dis.find(_.tag == Tag.SeriesInstanceUID).toRight(s"Cannot find SeriesInstanceUID of dose info series ${accessionNumber(dis)} @ selectDoseSeries"))
                eDiSeUid.map(diSeUid => 
                    findImages(cfind, diSeUid.value).map(itagss => 
                        val eiuids = itagss.traverse(tags =>
                            tags.find(_.tag == Tag.SOPInstanceUID).toRight(s"Cannot find SOPInstanceUID ${accessionNumberOfInfo(info)} @ selectDoseSeries"))
                        eiuids.map((info, _, manufacturer, model, attrWarns)))
                ).flatSequence
            }).flatSequence
        )

    // (Study Attributes, OCR results of dose series images)
    private def doOCR(doseUidsWithInfo: Seq[Either[String, (Seq[(Int, String)], Seq[StringTag], String, String, Seq[Int])]],
                        diMap: CTDoseInfo.CTInfo)(using cget: CGet) = 
        doseUidsWithInfo.traverse(eDoseUidWithInfo =>
                eDoseUidWithInfo.flatTraverse((info, imgUids, manufacturer, model, attrWarns) =>
                    val ereg = diMap.get((manufacturer, model))
                        .toRight(s"Cannot find information of $manufacturer.$model @ doOCR")
                        .flatMap(_.seriesInfo.regex match
                            case Some(s) if s.length > 0 =>
                                scala.util.matching.Regex(s).unanchored.asRight
                            case _ =>
                                s"Regex value not exist or empty $manufacturer.$model @ doOCR".asLeft
                        )
                    imgUids.traverse(imgUid =>  // try ocr of each image of dose series
                        val acno = info.find(_._1 == Tag.AccessionNumber).map(_._2).getOrElse(s"No Accession Number ${imgUid.value} @ doOCR")
                        getDicomStream(cget, imgUid.value).map(edis =>
                            edis.flatMap(dis =>
                                ereg.map(reg => 
                                    dicomReader.setInput(dis)
                                    val im = dicomReader.read(0, dicomReader.getDefaultReadParam())
                                    val ocrResult = Tesseract.doOCR(im)
                                    val finalResult = reg.findFirstMatchIn(ocrResult) match
                                        case Some(m) =>
                                            findFirstDoubleStringInMatch(m)
                                        case _ =>
                                            None
                                    val marked = drawString(im, s"$acno - ${finalResult.map(_.toString).getOrElse("NOT FOUND")}")
                                    DoseResultRaw(acno, finalResult, ocrResult, marked)
                                )
                            )
                        )
                    ).map(eImUids => 
                        eImUids.sequence.map((info, _, attrWarns)))
                )
            )

    def getCTDoses(config: Configuration.CTDoseConfig, diMap: CTDoseInfo.CTInfo, d: LocalDate = LocalDate.now(), collectTags: Seq[Int] = defaultCollectTags)
            :IO[(Seq[CTDoseResult], Seq[String])] = 
        val r = for 
            cfind <- findResource(config.connectionInfo)
            cget <- getResource(config.connectionInfo)
        yield (cfind, cget)

        r.use:
            case (cfind, cget) =>
                given CFind = cfind
                given CGet = cget
                val startTime = System.nanoTime()
                for
                    // CTs Tags : Seq[Seq[StringTag]]
                    ctTagss             <-  findCTStudies(cfind, d)
                    // Series Tags : (StudyUID, Seq of each SeriesTags) per Study
                    seTagss             <-  ctTagss.traverse(ctTags => gatherSeries(ctTags))
                    eattrs              <-  series2firstSeriesFirstImageAttributes(seTagss)
                    ctSeUidsWithAttrs   =   mergeSeriesWithAttributes(seTagss, eattrs, collectTags, config)
                    doseUidsWithInfo    <-  selectDoseSeries(ctSeUidsWithAttrs, diMap)
                    doseImagesWithInfo  <-  doOCR(doseUidsWithInfo, diMap)
                    (errs, bis)         = doseImagesWithInfo.partition(_.isLeft)
                yield {
                    val rights = bis.flatMap(_.toOption).map(t => CTDoseResult(t._1, t._2, t._3))
                    val lefts = errs.flatMap(_.swap.toOption)  // flatten RIGHTs and LEFTs
                    (rights, lefts)
                }
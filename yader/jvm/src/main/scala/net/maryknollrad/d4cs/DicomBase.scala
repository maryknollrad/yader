package net.maryknollrad.d4cs

import java.time.{LocalDate, LocalTime}
import java.time.format.DateTimeFormatter
import org.dcm4che3.data.*

object DicomBase:
    type QueryHandler[A] = (DicomTags, Attributes) => A
    case class StringTag(tag: Int, value: String):
        override def toString(): String = s"(${ElementDictionary.keywordOf(tag, null)}) : $value"
    type QueryTag = StringTag
    type RetrieveTag = Int
    type DicomTag = RetrieveTag | QueryTag
    type DicomTags = Seq[DicomTag]

    // used in CTDose
    type TagValue = StringTag
    type TagValues = Seq[TagValue]
    type DicomMap = Map[String, String]
    type QueryResult = (TagValues, DicomMap)
    
    private val DicomTimeFormat = DateTimeFormatter.ofPattern("HHmmss")
    given LocalDate2String:Conversion[LocalDate, String] = (d: LocalDate) => d.format(DateTimeFormatter.BASIC_ISO_DATE)
    given String2LocalDate:Conversion[String, LocalDate] = s => LocalDate.parse(s, DateTimeFormatter.BASIC_ISO_DATE)
    given String2LocalTime:Conversion[String, LocalTime] = s => LocalTime.parse(s, DicomTimeFormat)
    given Tup2StringAttribute: Conversion[Tuple2[Int, String], QueryTag] = t => StringTag(t._1, t._2)

    // CONSIDER : tag gathering level in CTDose.getDoses function
    // enum GatherLevel:
    //     case GatherNone, StudyLevel, SeriesLevel, ImageLevel

    import scala.jdk.CollectionConverters.*
    extension (attr: Attributes)
        def getNestedString(tags: Seq[Int]): Option[String] = 
            def h(a: Attributes, ts: Seq[Int]): Option[String] = 
                    if ts.length == 0 then
                        None
                    else if ts.length == 1 then
                        Option(a.getString(ts(0)))
                    else
                        val vr = ElementDictionary.vrOf(ts(0), attr.getPrivateCreator(ts(0)))
                        vr match
                            case VR.SQ => 
                                val sq = a.getSequence(ts(0))
                                sq.asScala.find(_.tags.contains(ts(1))).flatMap(h(_, ts.tail))
                            case _ =>
                                None
            h(attr, tags)

        def getNestedString(tagStringWithDotSeparator: String): Option[String] = 
            scala.util.Try:
                val tags = tagStringWithDotSeparator.split('.').map(ElementDictionary.tagForKeyword(_, null))
                getNestedString(tags)
            .toOption.flatten

trait DicomBase:
    import DicomBase.* 

    val deviceName: String
    val callingAe: String
    val calledAe: String
    val remoteHost: String
    val remotePort: Int

    protected val keys = Attributes()

    /* configureKeys */
    // CLI.addAttributes는 .으로 구분된 하나의 attribute를 추가하는 데 사용된다
    // int []tags로 되어있지만 여럿의 tag가 아니라 계층별 분리된 하나의 tag
    private def addAttributes(tagHierarchy: Seq[Int], value: Option[String]) = 
        var item: Attributes = keys
        tagHierarchy.init.foreach({ tag => 
            val pre_sq = item.getSequence(tag)
            val sq = if (pre_sq == null) item.newSequence(tag, 1) else pre_sq
            if (sq.isEmpty) sq.add(Attributes())
            item = sq.get(0)
        })
        val tag = tagHierarchy.last
        val vr = ElementDictionary.vrOf(tag, item.getPrivateCreator(tag))
        value match
            case Some(tv) => 
                item.setString(tag, vr, tv)
            case None => 
                if (vr == VR.SQ) item.newSequence(tag, 1).add(Attributes(0))
                else item.setNull(tag, vr)

    def addKeys(dtags: DicomTags) = 
        dtags.foreach: tag =>
            tag match 
                case QueryTag(t, v) =>
                    assert(!keys.contains(t))
                    addAttributes(Seq(t), Some(v))
                case t: RetrieveTag =>
                    assert(!keys.contains(t))
                    addAttributes(Seq(t), None)

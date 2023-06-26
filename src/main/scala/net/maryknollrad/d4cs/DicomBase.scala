package net.maryknollrad.d4cs

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import org.dcm4che3.data.*

object DicomBase:
    type QueryHandler[A] = (DicomTags, Attributes) => A
    case class QueryTag(tag: Int, value: String)
    type RetrieveTag = Int
    type DicomTag = RetrieveTag | QueryTag
    type DicomTags = Seq[DicomTag]
    private val dicomDatetimeFormat = DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
    given Date2String: Conversion[LocalDate, String] = (d: LocalDate) => d.format(DateTimeFormatter.BASIC_ISO_DATE)
    given Tup2StringAttribute: Conversion[Tuple2[Int, String], QueryTag] = t => QueryTag(t._1, t._2)
    given String2DateTime: Conversion[String, LocalDateTime] = s => LocalDateTime.parse(s, dicomDatetimeFormat)

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
                    addAttributes(Seq(t), Some(v))
                case t: RetrieveTag =>
                    addAttributes(Seq(t), None)

package net.maryknollrad.ctdose

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import cats.syntax.all.* 
import org.dcm4che3.data.ElementDictionary
import javax.lang.model.element.Element
import scala.util.chaining.* 
import org.dcm4che3.data.{VR, Tag}
import java.time.{LocalDate, LocalTime}
import doobie.implicits.javatimedrivernative._
import DB.*
import java.time.format.DateTimeFormatter

object SQLite:
    val xa = Transactor.fromDriverManager[IO](
        "org.sqlite.JDBC", "jdbc:sqlite:ctdose.db", None
    )

    // TODO: 연달아 대문자면 오류!
    def toLowerCaseFieldName(s: String) = 
        s.head.toLower +: s.tail.map(_ match 
            case ' ' => "_"
            case ch if ch.isUpper => s"_${ch.toLower}"
            case ch => ch
        ).mkString("")
    
    def createStudyTableSQL(tags: Seq[Int]) = 
        // first 4 are accession number, patient related 3 fields and exclude institution name
        val midFields = tags.drop(4).withFilter(_ != Tag.InstitutionName).map(t => 
            val fname = ElementDictionary.keywordOf(t, null).pipe(toLowerCaseFieldName)
            val ftype = ElementDictionary.vrOf(t, null) match
                case VR.DA => "DATE"
                case VR.TM => "TIME"
                case _ => "TEXT"
            s"$fname $ftype"
        )
        val createSQL = """
        | CREATE TABLE IF NOT EXISTS study
        | (acno TEXT NOT NULL, patientid TEXT NOT NULL REFERENCES patient (id),
        | studydate DATE NOT NULL, studytime TIME NOT NULL, studydescription TEXT NOT NULL, protocol TEXT, 
        | bodypart TEXT NOT NULL, manufacturer TEXT NOT NULL, modelname TEXT NOT NULL, 
        | station TEXT NOT NULL, operator TEXT NOT NULL, dosevalue1 REAL NOT NULL DEFAULT 0.0, dosevalue2 REAL NOT NULL DEFAULT 0.0)
        """.stripMargin
        Fragment.const(createSQL)

    def createTablesIfNotExists(tags: Seq[Int]) = 
        val study = createStudyTableSQL(tags).update.run

        val patient = sql"""
        | CREATE TABLE IF NOT EXISTS patient 
        | (id TEXT NOT NULL PRIMARY KEY, sex TEXT NOT NULL, birthday DATE NOT NULL)
        """.stripMargin.update.run

        val log = sql"""
        | CREATE TABLE IF NOT EXISTS log 
        | (at DATETIME NOT NULL DEFAULT (datetime('now', 'localtime')), 
        | logtype INTEGER NOT NULL DEFAULT 0, content TEXT NOT NULL)
        """.stripMargin.update.run

        (patient, log, study).mapN(_ + _ + _).transact(xa)

    def log(msg: String, ltype: DB.LogType) = 
        sql"""INSERT INTO log (logtype, content) VALUES (${ltype.ordinal}, $msg)""".update.run.transact(xa)

    private val logDateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    def updateLastDateProcessed(d: LocalDate) = 
        log(logDateTimeFormatter.format(d), DB.LogType.LastProcessedDate)

    def getLastDateProcessed() = 
        val typeInt = DB.LogType.LastProcessedDate.ordinal
        sql"""SELECT content FROM log 
        | WHERE logtype = $typeInt and 
        | at = (SELECT max(at) FROM log WHERE logtype = $typeInt)""".stripMargin
            .query[String]
            .map(s => logDateTimeFormatter.parse(s).pipe(LocalDate.from))
            .option.transact(xa)

    def getCountAndDoseSum() =
        sql"SELECT count(*), sum(dosevalue1), sdate FROM study, (SELECT min(studydate) as sdate FROM study)".query[(Int, Double, String)].unique.transact(xa)

    def getLastLogs(ltypes: Seq[DB.LogType] = Seq.empty, count: Int = 10) = 
        assert(count > 0)
        val where = 
            Option.when(ltypes.nonEmpty) :
                val within = ltypes.map(_.ordinal).mkString("WHERE logtype IN (", ",", ")")
                Fragment(within, List.empty)
            .getOrElse(Fragment.empty)
        val q  = sql"SELECT logtype, content FROM log $where ORDER BY at DESC LIMIT $count"
        q.query[(Int, String)].to[List].transact(xa)

    def insertStudyAndPatient(study: Study, patient: Patient) = 
        val studyInsert = sql"""INSERT INTO study VALUES ($study)""".update.run
        val patientInsert = sql"""INSERT INTO patient VALUES ($patient) ON CONFLICT DO NOTHING""".update.run
        patientInsert.combine(studyInsert).transact(xa)

    private def intervalConst(qi: QueryInterval) = 
        import DB.QueryInterval.* 
        qi match
            case Day =>
                Fragment.const0("'%j'")
            case Week =>
                Fragment.const0("'%W'")
            case Month =>
                Fragment.const0("'%m'")
            case Year =>
                Fragment.const0("'%Y'")

    def getBodypartCounts(interval: QueryInterval, from: Int = 1, to: Int = 0) = 
        val itv = intervalConst(interval)
        fr"""SELECT bodypart, cast(strftime($itv, studydate) as integer) as stime, count(*) as bcount 
        |FROM study, (SELECT cast(strftime($itv, datetime('now', 'localtime')) as integer) as tnum) 
        |WHERE stime BETWEEN (tnum - $from) AND (tnum - $to) AND bodypart NOT LIKE '*%' 
        |GROUP BY bodypart ORDER BY bcount DESC""".stripMargin
            .query[(String, Int, Long)].to[List].transact(xa)

    def partitionedQuery(partition: QueryPartition, interval: QueryInterval, subpartition: Option[String] = None, from: Int = 1, to: Int = 0) = 
        val pfrag = Fragment.const0(partition.strValue)
        val itv = intervalConst(interval)

        val subFragged = subpartition.map(p => fr"AND $pfrag = $p").getOrElse(Fragment.empty)
        val qSql = fr"""SELECT $pfrag, studydate, cast(strftime($itv, studydate) as integer) as stime, acno, patientid, 
            |dosevalue1, dosevalue2, rank() OVER (PARTITION BY $pfrag, cast(strftime($itv, studydate) as integer) ORDER BY dosevalue1) FROM study,
            |(SELECT cast(strftime($itv, datetime('now', 'localtime')) as integer) AS tnum) 
            |WHERE stime BETWEEN (tnum - $from) AND (tnum - $to) $subFragged""".stripMargin

        qSql.query[Partitioned].to[List].transact(SQLite.xa)
        
/* QUERIES
select body_part_examined, dose_value from study order by body_part_examined, dose_value;
select body_part_examined, avg(dose_value) from study group by body_part_examined;
select cast(strftime('%m', date('now')) as integer) / 4, strftime('%m', date('now'), strftime('%W', date('now'));
select body_part_examined as bpart, dose_value, strftime('%W', study_date) as sweek, row_number() over (partition by body_part_examined, strftime('%W', study_date) order by dose_value) from study;
*/

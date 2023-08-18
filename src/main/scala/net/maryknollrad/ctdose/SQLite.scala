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

    def log(msg: String, ltype: Int = 0) = 
        sql"""INSERT INTO log (logtype, content) VALUES ($ltype, $msg)""".update.run

    private val logDateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    private val procedureSaveLogType = 1

    def updateLastDateProcessed(d: LocalDate) = 
        log(logDateTimeFormatter.format(d), procedureSaveLogType).transact(xa)

    def getLastDateProcessed() = 
        sql"""SELECT content FROM log 
        | WHERE logtype = $procedureSaveLogType and 
        | at = (SELECT max(at) FROM log WHERE logtype = $procedureSaveLogType)""".stripMargin
            .query[String]
            .map(s => logDateTimeFormatter.parse(s).pipe(LocalDate.from))
            .option.transact(xa)
        
    def insertStudyAndPatient(study: Study, patient: Patient) = 
        val studyInsert = sql"""INSERT INTO study VALUES ($study)""".update.run
        val patientInsert = sql"""INSERT INTO patient VALUES ($patient) ON CONFLICT DO NOTHING""".update.run
        patientInsert.combine(studyInsert).transact(xa)

    def partitionedQuery(partition: QueryPartition, interval: QueryInterval, subpartition: Option[String] = None, from: Int = 1, to: Int = 0) = 
        import DB.QueryPartition.* 
        import DB.QueryInterval.* 

        val pfrag = Fragment.const0(partition.strValue)
        val itv = interval match
            case Day =>
                Fragment.const0("'%j'")
            case Week =>
                Fragment.const0("'%W'")
            case Month =>
                Fragment.const0("'%m'")
        val qSql = fr"""SELECT $pfrag, cast(strftime($itv, studydate) as integer) as stime, acno, patientid, 
            |dosevalue1, dosevalue2, rank() OVER (PARTITION BY $pfrag, cast(strftime($itv, studydate) as integer) ORDER BY dosevalue1) FROM study,
            |(SELECT cast(strftime($itv, datetime('now', 'localtime')) as integer) AS tnum) 
            |WHERE stime >= (tnum - $from) AND stime < (tnum - $to)""".stripMargin
        val subFragged = subpartition.map(p => qSql ++ fr"AND $pfrag = $p").getOrElse(qSql)

        subFragged.query[Partitioned]
        
/* QUERIES
select body_part_examined, dose_value from study order by body_part_examined, dose_value;
select body_part_examined, avg(dose_value) from study group by body_part_examined;
select cast(strftime('%m', date('now')) as integer) / 4, strftime('%m', date('now'), strftime('%W', date('now'));
select body_part_examined as bpart, dose_value, strftime('%W', study_date) as sweek, row_number() over (partition by body_part_examined, strftime('%W', study_date) order by dose_value) from study;
*/

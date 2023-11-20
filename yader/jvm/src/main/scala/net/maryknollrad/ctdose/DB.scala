package net.maryknollrad.ctdose

import java.time.{LocalDate, LocalTime}
import doobie.postgres.implicits.*

object DB:
    case class Study(accessionNumber: String, patientId: String, studyDate: LocalDate, studyTime: LocalTime, 
        description: String, alternative: String, bodypart: String, manufacturer: String, model: String, 
        station: String, operator: String, physician: String, dose1: Double, dose2: Double)

    case class Patient(id: String, sex: String, birthday: LocalDate)

    enum QueryInterval(val strValue: String):
        case Day extends QueryInterval("day")
        case Week extends QueryInterval("week")
        case Month extends QueryInterval("month")
        case Year extends QueryInterval("year")
        
    object QueryInterval:
        val qiSize = QueryInterval.values.length
        def defaultRange(qi: QueryInterval) = if qi == QueryInterval.Day then (1, 0) else (0, 0)

    enum QueryPartition(val strValue: String):
        case Bodypart extends QueryPartition("bodypart")
        // case Operator extends QueryPartition("operator")
        // case Physician extends QueryPartition("physician")

    case class Partitioned(part: String, studydate: String, dateNumber: Int, accessionNumber: String, patientId: String, 
        dose1: Double, dose2: Double, rank: Int) derives upickle.default.ReadWriter

    enum LogType:
        case  LastProcessedDate, Debug, Info, Warn, Error

trait DB:
    import cats.effect.IO
    import doobie.*
    import doobie.implicits.*
    import cats.syntax.all.* 
    import java.time.format.DateTimeFormatter
    import scala.util.chaining.* 
    import DB.* 

    val xa: Transactor.Aux[IO, Unit]
    val datetimeType: String
    val now: String
    // select min(studydate) as string 
    // sqlite : no need to convert, postgresql : use to_char
    val minStudyDateAsString: String    
    def intervals(value: String): Seq[Fragment]
    lazy val timeIntervals = intervals(now).zip(intervals("studydate"))
    // assert(timeIntervals.length == QueryInterval.qiSize)

    protected val createStudiesSQL = Fragment.const("""CREATE TABLE IF NOT EXISTS studies
        | (acno TEXT NOT NULL, patientid TEXT NOT NULL REFERENCES patients (id),
        | studydate DATE NOT NULL, studytime TIME NOT NULL, studydescription TEXT NOT NULL, alternative TEXT NOT NULL, 
        | bodypart TEXT NOT NULL, manufacturer TEXT NOT NULL, modelname TEXT NOT NULL, station TEXT NOT NULL, 
        | operator TEXT NOT NULL, physician TEXT NOT NULL, dosevalue1 REAL NOT NULL DEFAULT 0.0, dosevalue2 REAL NOT NULL DEFAULT 0.0)
        """.stripMargin).update.run

    protected val createPatientsSQL = Fragment.const("""CREATE TABLE IF NOT EXISTS patients
        | (id TEXT NOT NULL PRIMARY KEY, sex TEXT NOT NULL, birthday DATE NOT NULL)""".stripMargin).update.run

    protected lazy val createLogsSQL = 
        Fragment.const(s"""CREATE TABLE IF NOT EXISTS logs  
        | (logtime $datetimeType NOT NULL DEFAULT ($now), 
        | logtype INTEGER NOT NULL DEFAULT 0, content TEXT NOT NULL)""".stripMargin).update.run

    def createTablesIfNotExists() = 
        (createPatientsSQL, createLogsSQL, createStudiesSQL).mapN(_ + _ + _).transact(xa)

    def log(msg: String, ltype: DB.LogType) = 
        sql"""INSERT INTO logs (logtype, content) VALUES (${ltype.ordinal}, $msg)""".update.run.transact(xa)

    def logs(msgs: Seq[String], ltype: DB.LogType) = 
        val iq = """INSERT INTO logs (logtype, content) VALUES (?, ?)"""
        val inserts = msgs.map((ltype.ordinal, _))
        Update[(Int, String)](iq).updateMany(inserts).transact(xa)

    private val logDateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    def updateLastDateProcessed(d: LocalDate) = 
        log(logDateTimeFormatter.format(d), DB.LogType.LastProcessedDate)

    def getLastDateProcessed() = 
        val typeInt = DB.LogType.LastProcessedDate.ordinal
        sql"""SELECT content FROM logs 
        | WHERE logtype = $typeInt and 
        | logtime = (SELECT max(logtime) FROM logs WHERE logtype = $typeInt)""".stripMargin
            .query[String]
            .map(s => logDateTimeFormatter.parse(s).pipe(LocalDate.from))
            .option.transact(xa)

    def getCountAndDoseSum() =
        val s = Fragment.const(s"SELECT count(*), sum(dosevalue1), (SELECT $minStudyDateAsString as mindate FROM studies) FROM studies")
        s.query[(Int, Double, String)].unique.transact(xa)

    def getLastLogs(ltypes: Seq[DB.LogType] = Seq.empty, count: Int = 10) = 
        assert(count > 0)
        val where = 
            Option.when(ltypes.nonEmpty) :
                val within = ltypes.map(_.ordinal).mkString("WHERE logtype IN (", ",", ")")
                Fragment(within, List.empty)
            .getOrElse(Fragment.empty)
        val q  = sql"SELECT logtype, content FROM logs $where ORDER BY logtime DESC LIMIT $count"
        q.query[(Int, String)].to[List].transact(xa)

    def insertStudyAndPatient(study: Study, patient: Patient) = 
        val studyInsert = sql"""INSERT INTO studies VALUES 
            | (${study.accessionNumber}, ${study.patientId}, ${study.studyDate}, ${study.studyTime},
            | ${study.description}, ${study.alternative}, ${study.bodypart}, ${study.manufacturer},
            | ${study.model}, ${study.station}, ${study.operator}, ${study.physician},
            | ${study.dose1}, ${study.dose2})""".stripMargin.update.run
        val patientInsert = sql"""INSERT INTO patients VALUES (${patient.id}, ${patient.sex}, ${patient.birthday}) ON CONFLICT DO NOTHING""".update.run
        patientInsert.combine(studyInsert).transact(xa)

    def getBodypartCounts(interval: QueryInterval, from: Int = 1, to: Int = 0) = 
        val (today, studydate) = timeIntervals(interval.ordinal)
        val sql = fr"""SELECT bodypart, count(*) as bcount 
        |FROM studies, (SELECT $today as today) as t 
        |WHERE $studydate BETWEEN (t.today - $from) AND (t.today - $to) AND bodypart NOT LIKE '*%' 
        |GROUP BY bodypart ORDER BY bcount DESC""".stripMargin
        sql.query[(String, Long)].to[List].transact(xa)

    def partitionedQuery(partition: QueryPartition, interval: QueryInterval, subpartition: Option[String] = None, from: Int = 1, to: Int = 0) = 
        val pfrag = Fragment.const0(partition.strValue)
        val (today, studydate) = timeIntervals(interval.ordinal)

        val subFragged = subpartition.map(p => fr"AND $pfrag = $p").getOrElse(Fragment.empty)
        val qSql = fr"""SELECT $pfrag, studydate, $studydate as stime, acno, patientid, 
            |dosevalue1, dosevalue2, rank() OVER (PARTITION BY $pfrag, $studydate ORDER BY dosevalue1) FROM studies,
            |(SELECT $today as today) as t 
            |WHERE $studydate BETWEEN (t.today - $from) AND (t.today - $to) $subFragged""".stripMargin
        qSql.query[Partitioned].to[List].transact(xa)

    def getStudies() = 
        sql"SELECT distinct studydescription, bodypart FROM studies ORDER BY studydescription"
            .query[(String, String)].to[List].transact(xa)
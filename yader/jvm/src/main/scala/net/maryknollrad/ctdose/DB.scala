package net.maryknollrad.ctdose

import java.time.{LocalDate, LocalTime}
import doobie.postgres.implicits.*

object DB:
    case class Study(accessionNumber: String, patientId: String, studyDate: LocalDate, studyTime: LocalTime, 
        description: String, alternative: String, bodypart: String, manufacturer: String, model: String, 
        station: String, operator: String, physician: String, dose1: Double, dose2: Double)

    case class Patient(id: String, sex: String, birthday: LocalDate)

    import net.maryknollrad.yader.Constants.{queryIntervals => qi}

    enum QueryInterval(val strValue: String):
        case Day extends QueryInterval(qi(0))
        case Week extends QueryInterval(qi(1))
        case Month extends QueryInterval(qi(2))
        case Year extends QueryInterval(qi(3))
        
    object QueryInterval:
        val qiSize = QueryInterval.values.length
        // def defaultRange(qi: QueryInterval) = if qi == QueryInterval.Day then (1, 0) else (0, 0)
        def defaultRange(qi: QueryInterval) = (1, 0)
        
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
    def datetimeType: String
    def now: String
    def serial: String
    def age(f1: String, f2: String): String    // calculate age
    def daysbeforetoday(n: Int): String

    // select min(studydate) as string 
    // sqlite : no need to convert, postgresql : use to_char
    val minStudyDateAsString: String   
    val maxInteger = Integer.MAX_VALUE  // postgresql uses 4 bytes, mysql can store larger number

    // extract subfield from timestamp or date field, functions depends on DB
    def subtime(value: String): Seq[Fragment]

    // sql fragments, according to QueryIntervals
    lazy val timeIntervals = subtime(now).zip(subtime("studydate"))
    assert(timeIntervals.length == QueryInterval.qiSize)

    protected val createStudiesSQL = Fragment.const("""CREATE TABLE IF NOT EXISTS studies
        | (acno TEXT NOT NULL, patientid TEXT NOT NULL REFERENCES patients (id),
        | sid INTEGER REFERENCES ctstudies (sid), studydate DATE NOT NULL, studytime TIME NOT NULL, alternative TEXT NOT NULL, 
        | bodypart TEXT NOT NULL, manufacturer TEXT NOT NULL, modelname TEXT NOT NULL, station TEXT NOT NULL, 
        | operator TEXT NOT NULL, physician TEXT NOT NULL, dosevalue1 REAL NOT NULL DEFAULT 0.0, dosevalue2 REAL NOT NULL DEFAULT 0.0)
        """.stripMargin).update.run

    protected val createStudiesStudydateIndexSQL = 
        fr"CREATE INDEX IF NOT EXISTS studyindex ON studies (studydate)".update.run

    protected val createPatientsSQL = Fragment.const("""CREATE TABLE IF NOT EXISTS patients
        | (id TEXT NOT NULL PRIMARY KEY, sex TEXT NOT NULL, birthday DATE NOT NULL)""".stripMargin).update.run

    protected val createCTStudiesSQL = Fragment.const(s"""CREATE TABLE IF NOT EXISTS ctstudies
        | (sid $serial, studyname TEXT NOT NULL UNIQUE, dummy BOOL NOT NULL DEFAULT false)""".stripMargin).update.run

    protected val createCategoriesSQL = 
        Fragment.const(s"CREATE TABLE IF NOT EXISTS categories (cid $serial, category TEXT NOT NULL UNIQUE)").update.run

    protected val createDrlsSQL = Fragment.const(s"""CREATE TABLE IF NOT EXISTS drls
        | (did $serial, cid INTEGER REFERENCES categories (cid), label TEXT NOT NULL, ctdi REAL NOT NULL, dlp REAL NOT NULL,
        | minage INTEGER NOT NULL DEFAULT 0, maxage INTEGER NOT NULL DEFAULT $maxInteger, dorder INTEGER NOT NULL)""".stripMargin).update.run

    protected val createCTDrlJoinSQL = Fragment.const("""CREATE TABLE IF NOT EXISTS ctdrljoin 
        | (sid INTEGER REFERENCES ctstudies (sid), cid INTEGER REFERENCES categories (cid), did INTEGER REFERENCES drls (did), UNIQUE (sid, cid, did))""".stripMargin).update.run

    protected lazy val createLogsSQL = 
        Fragment.const(s"""CREATE TABLE IF NOT EXISTS logs  
        | (logtime $datetimeType NOT NULL DEFAULT ($now), 
        | logtype INTEGER NOT NULL DEFAULT 0, content TEXT NOT NULL, UNIQUE (logtype, logtime))""".stripMargin).update.run

    protected val createLogsIndexSQL = 
        fr"CREATE INDEX IF NOT EXISTS logindex ON logs (logtype, logtime)".update.run

    def createTablesIfNotExists() = 
        // (createPatientsSQL, createLogsSQL, createCTStudiesSQL, createStudiesSQL).mapN(_ + _ + _ + _).transact(xa)
        Seq(createPatientsSQL, createCTStudiesSQL, createLogsSQL, createLogsIndexSQL, createStudiesSQL, createStudiesStudydateIndexSQL, 
            createCategoriesSQL, createDrlsSQL, createCTDrlJoinSQL).reduce(_ *> _).transact(xa)
            //.zipWithIndex.map(t => IO.println(s"${t._2}") *> t._1.transact(xa)).sequence

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
        val patientInsert = sql"""INSERT INTO patients VALUES (${patient.id}, ${patient.sex}, ${patient.birthday}) ON CONFLICT DO NOTHING""".update.run
        val getSid = sql"""INSERT INTO ctstudies (studyname) VALUES (${study.description})
            | ON CONFLICT (studyname) DO UPDATE SET dummy = true RETURNING sid""".stripMargin.query[Int].unique
        def studyInsert(sid: Int) = sql"""INSERT INTO studies (acno, patientid, studydate, studytime, sid, alternative, bodypart, 
            | manufacturer, modelname, station, operator, physician, dosevalue1, dosevalue2) VALUES 
            | (${study.accessionNumber}, ${study.patientId}, ${study.studyDate}, ${study.studyTime},
            | ${sid}, ${study.alternative}, ${study.bodypart}, ${study.manufacturer},
            | ${study.model}, ${study.station}, ${study.operator}, ${study.physician},
            | ${study.dose1}, ${study.dose2})""".stripMargin.update.run
        val sinsert = getSid.flatMap(sid => studyInsert(sid))
        patientInsert.combine(sinsert).transact(xa)

    private def timecondition(interval: QueryInterval, from: Int, to: Int): Fragment = 
        val unit = interval match
            case QueryInterval.Day => 1
            case QueryInterval.Week => 7
            case QueryInterval.Month => 30
            case QueryInterval.Year => 365
        val (d1, d2) = (Fragment.const(daysbeforetoday(unit * from)), Fragment.const(daysbeforetoday(unit * to)))
        fr"studydate BETWEEN $d1 AND $d2"

    def getBodypartCounts(interval: QueryInterval, from: Int = 1, to: Int = 0) = 
        // two sql fragments that extracts subfields of time
        val (today, studyperiod) = timeIntervals(interval.ordinal)
        /*
        sql"""SELECT bodypart, count(*) as bcount 
        |FROM studies, (SELECT $today as today) as t 
        |WHERE $studyperiod BETWEEN (t.today - $from) AND (t.today - $to) AND bodypart NOT LIKE '*%' 
        |GROUP BY bodypart ORDER BY bcount DESC""".stripMargin
        */
        sql"""SELECT bodypart, count(*) as bcount 
        |FROM studies
        |WHERE ${timecondition(interval, from, to)} AND bodypart NOT LIKE '*%' 
        |GROUP BY bodypart ORDER BY bcount DESC""".stripMargin
            .query[(String, Long)].to[List].transact(xa)

    def partitionedQuery(partition: QueryPartition, interval: QueryInterval, subpartition: Option[String] = None, from: Int = 1, to: Int = 0) = 
        val pfrag = Fragment.const0(partition.strValue)
        val (today, studyperiod) = timeIntervals(interval.ordinal)
        val subFragged = subpartition.map(p => fr"AND $pfrag = $p").getOrElse(Fragment.empty)
        /*
        sql"""SELECT $pfrag, studydate, $studyperiod as stime, acno, patientid, 
            | dosevalue1, dosevalue2, rank() OVER (PARTITION BY $pfrag, $studyperiod ORDER BY dosevalue1) FROM studies,
            | (SELECT $today as today) as t 
            | WHERE $studyperiod BETWEEN (t.today - $from) AND (t.today - $to) $subFragged""".stripMargin
                .query[Partitioned].to[List].transact(xa)
        */
        sql"""SELECT $pfrag, studydate, $studyperiod as stime, acno, patientid, 
            | dosevalue1, dosevalue2, rank() OVER (PARTITION BY $pfrag, $studyperiod ORDER BY dosevalue1) FROM studies
            | WHERE ${timecondition(interval, from, to)} $subFragged""".stripMargin
                .query[Partitioned].to[List].transact(xa)

    def getStudies(category: String = "korea"): IO[List[(String, Int, String)]] = 
        sql"""SELECT studyname, ctstudies.sid, label FROM ctstudies JOIN ctdrljoin ON ctstudies.sid = ctdrljoin.sid  
        | JOIN drls ON ctdrljoin.did = drls.did JOIN categories ON drls.cid = categories.cid WHERE categories.category = $category ORDER BY ctstudies.sid""".stripMargin
            .query[(String, Int, String)].to[List].transact(xa)

    def getCategories(): IO[List[(Int, String)]] = 
        sql"SELECT cid, category FROM categories ORDER BY cid".query[(Int, String)].to[List].transact(xa)

    // cannot use DISTINCT with ORDER BY
    def getLabels(category: String = "korea"): IO[List[String]] = 
        sql"""SELECT label, MAX(dorder) FROM drls JOIN categories ON drls.cid = categories.cid 
        | WHERE categories.category = $category GROUP BY label ORDER BY max(dorder)""".stripMargin
            .query[(String, Int)].map(_._1).to[List].transact(xa)

    def insertCategory(category: String) = 
        val insertCategory = sql"INSERT INTO categories (category) VALUES ($category) RETURNING cid".query[Int].unique
        def insertNoneDrl(cid: Int) = sql"INSERT INTO drls (cid, label, ctdi, dlp, dorder) VALUES ($cid, 'NONE', 0, 0, 0) RETURNING did".query[Int].unique
        def setDefaultsToNone(cid: Int, did: Int) = sql"INSERT INTO ctdrljoin SELECT sid, $cid, $did FROM ctstudies".update.run
        val run = for 
            cid     <-  insertCategory
            did     <-  insertNoneDrl(cid)
            count   <-  setDefaultsToNone(cid, did)
        yield count
        run.transact(xa)

    def insertDrl(category: String, dorder: Int, label: String, ctdi: Double, dlp: Double, minAge: Int = 0, maxAge: Int = Int.MaxValue) = 
        sql"""INSERT INTO drls (cid, label, ctdi, dlp, minage, maxage, dorder) 
        | SELECT cid, $label, $ctdi, $dlp, $minAge, $maxAge, $dorder FROM categories WHERE category = $category""".stripMargin
            .update.run.transact(xa)

    def updateDrl(cid: Int, sid: Int, dlabel: String) = 
        sql"""WITH drldid AS
        | (SELECT did FROM drls WHERE cid = $cid AND label = $dlabel) 
        | UPDATE ctdrljoin SET did = drldid.did FROM drldid WHERE cid = $cid and sid = $sid""".stripMargin
            .update.run.transact(xa)

    protected val patientAgeStr = age("s.studydate", "p.birthday")
    protected lazy val fromDrljoinFrag = Fragment.const(s"""FROM studies s 
                                        | JOIN ctdrljoin c ON s.sid = c.sid
                                        | JOIN patients p ON s.patientid = p.id
                                        | JOIN drls d1 ON c.did = d1.did 
                                        | JOIN drls d2 ON d1.label = d2.label 
                                        |       AND $patientAgeStr >= d2.minage 
                                        |       AND $patientAgeStr < d2.maxage""".stripMargin)

    def drlSummary(cid: Int, interval: QueryInterval, from: Int = 1, to: Int = 0) = 
        val (today, studyperiod) = timeIntervals(interval.ordinal)
        /*
        sql"""SELECT d2.label, $studyperiod as stime, s.dosevalue1 <= d2.dlp AS doseflag, count(*) $fromDrljoinFrag, 
            | (SELECT $today AS today) AS t 
            | WHERE c.cid = $cid AND
            | $studyperiod BETWEEN (t.today - $from) AND (t.today - $to)
            | GROUP BY d2.label, $studyperiod, doseflag""".stripMargin
                .query[(String, String, Boolean, Int)].to[List].transact(xa)
        */
        sql"""SELECT d2.label, $studyperiod as stime, s.dosevalue1 <= d2.dlp AS doseflag, count(*) $fromDrljoinFrag
            | WHERE c.cid = $cid AND
            | ${timecondition(interval, from, to)}
            | GROUP BY d2.label, $studyperiod, doseflag""".stripMargin
                .query[(String, String, Boolean, Int)].to[List].transact(xa)

    def drlFull(cid: Int, interval: QueryInterval, from: Int = 1, to: Int = 0) = 
        val (today, studyperiod) = timeIntervals(interval.ordinal)
        val patientAgeFrag = Fragment.const(age("s.studydate", "p.birthday"))
        /*
        sql"""SELECT d2.label, s.acno, s.patientid, $studyperiod AS stime, 
            | $patientAgeFrag as age, s.dosevalue1, d2.ctdi, d2.dlp, s.dosevalue1 < d2.dlp AS doseflag, 
            | rank() OVER (PARTITION BY d2.did ORDER BY s.dosevalue1) $fromDrljoinFrag,
            | (SELECT $today AS today) AS t 
            | WHERE c.cid = $cid AND d1.label != 'NONE' AND
            | $studyperiod BETWEEN (t.today - $from) AND (t.today - $to)""".stripMargin
                .query[(String, String, String, String, String, Double, Double, Double, Boolean, Int)].to[List].transact(xa)
        */
        sql"""SELECT d2.label, s.acno, s.patientid, $studyperiod AS stime, 
            | $patientAgeFrag as age, s.dosevalue1, d2.ctdi, d2.dlp, s.dosevalue1 < d2.dlp AS doseflag, 
            | rank() OVER (PARTITION BY d2.did ORDER BY s.dosevalue1) $fromDrljoinFrag
            | WHERE c.cid = $cid AND d1.label != 'NONE' AND
            | ${timecondition(interval, from, to)}""".stripMargin
                .query[(String, String, String, String, String, Double, Double, Double, Boolean, Int)].to[List].transact(xa)

    def bodypartCoverage(cid: Int, interval: QueryInterval, from: Int = 1, to: Int = 0) = 
        val (today, studyperiod) = timeIntervals(interval.ordinal)
        /*
        sql"""SELECT s.bodypart, $studyperiod AS stime, d.label != 'NONE' AS coverflag, count(*) 
            | FROM studies s 
	        | JOIN ctdrljoin c ON s.sid = c.sid
        	| JOIN drls d ON c.did = d.did,
            | (SELECT $today AS today) AS t 
            | WHERE c.cid = $cid AND
            | $studyperiod BETWEEN (t.today - $from) AND (t.today - $to)
            | GROUP BY s.bodypart, stime, coverflag""".stripMargin
                .query[(String, String, Boolean, Int)].to[List].transact(xa)
        */
        sql"""SELECT s.bodypart, $studyperiod AS stime, d.label != 'NONE' AS coverflag, count(*) 
            | FROM studies s 
	        | JOIN ctdrljoin c ON s.sid = c.sid
        	| JOIN drls d ON c.did = d.did
            | WHERE c.cid = $cid AND
            | ${timecondition(interval, from, to)}
            | GROUP BY s.bodypart, stime, coverflag""".stripMargin
                .query[(String, String, Boolean, Int)].to[List].transact(xa)

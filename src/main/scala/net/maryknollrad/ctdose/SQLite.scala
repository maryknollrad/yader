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
        // val createSQL = """
        // | CREATE TABLE IF NOT EXISTS study
        // | (acno TEXT NOT NULL, patient_id TEXT NOT NULL REFERENCES patient (id),
        // """.stripMargin ++ midFields.mkString(", ") ++ """,
        // | dose_value REAL NOT NULL DEFAULT 0.0)
        // """.stripMargin
        val createSQL = """
        | CREATE TABLE IF NOT EXISTS study
        | (acno TEXT NOT NULL, patient_id TEXT NOT NULL REFERENCES patient (id),
        | study_date DATE NOT NULL, study_time TIME NOT NULL, study_description TEXT NOT NULL, protocol_name TEXT NOT NULL, 
        | body_part_examined TEXT NOT NULL, manufacturer TEXT NOT NULL, manufacturer_model_name TEXT NOT NULL, 
        | station_name TEXT NOT NULL, operators_name TEXT NOT NULL, dose_value REAL NOT NULL DEFAULT 0.0)
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
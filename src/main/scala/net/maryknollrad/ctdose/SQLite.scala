package net.maryknollrad.ctdose

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import cats.syntax.all.* 
import org.dcm4che3.data.ElementDictionary
import javax.lang.model.element.Element
import scala.util.chaining.* 
import org.dcm4che3.data.VR
import java.time.{LocalDate, LocalTime}
import doobie.implicits.javatimedrivernative._
import net.maryknollrad.ctdose.CTDoseInfo.DBField

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
        val midFields = tags.drop(4).map(t => 
            val fname = ElementDictionary.keywordOf(t, null).pipe(toLowerCaseFieldName)
            val ftype = ElementDictionary.vrOf(t, null) match
                case VR.DA => "DATE"
                case VR.TM => "TIME"
                case _ => "TEXT"
            s"$fname $ftype"
        )
        val createSQL = """
        | CREATE TABLE IF NOT EXISTS study
        | (acno TEXT NOT NULL, patient_id TEXT NOT NULL REFERENCES patient (id),
        """.stripMargin ++ midFields.mkString(", ") ++ """,
        | dose_value REAL NOT NULL DEFAULT 0.0, img BLOB)
        """.stripMargin
        Fragment.const(createSQL)

    def createTablesIfNotExists(tags: Seq[Int]) = 
        val study = createStudyTableSQL(tags).update.run

        val patient = sql"""
        | CREATE TABLE IF NOT EXISTS patient 
        | (id TEXT NOT NULL PRIMARY KEY, sex TEXT, birthday DATE)
        """.stripMargin.update.run

        val log = sql"""
        | CREATE TABLE IF NOT EXISTS log 
        | (at DATETIME NOT NULL DEFAULT current_timestamp, 
        | logtype INTEGER NOT NULL DEFAULT 0, content TEXT NOT NULL)
        """.stripMargin.update.run

        (patient, log, study).mapN(_ + _ + _).transact(xa)
        // patient.combine(log).combine(study).transact(xa)
        // patient.combine(log).transact(xa)

    def log(msg: String, ltype: Int = 0) = 
        sql"""
        | INSERT INTO log (logtype = $ltype, content = $msg)
        """.stripMargin.update.run

    def insertStudyAndPatient(study: Tuple, patient: Tuple) = 
        /* given Write[DBField] = Write[DBField].contramap((f: DBField) =>
                f match
                    case v: String => v // Write[String]
                    case v: Double => v // Write[Double]
                    case v: LocalDate => v // rite[LocalDate]
                    case v: LocalTime => v // Write[LocalTime]
                    case v: Option[Array[Byte]] => v // Write[Option[Array[Byte]]]
            )
        given sDbWriter: Write[Seq[DBField]] = Write[Seq[DBField]].contramap(dbs => dbs)
        */
        val sql = "INSERT INTO study VALUES (" ++ Range(0, study.size).map(_ => "?").mkString(", ") ++ ")"
        val studyInsert = HC.prepareStatement(sql)(HPS.set(study))

        // val patientInsert = sql"""
        // | INSERT INTO patient VALUES (${patient(0)}, ${patient(1)}, ${patient(2)})
        // | ON CONFLICT DO NOTHING
        // """.stripMargin.update.run        
        val patientInsert = HC.prepareStatement("""INSERT INTO patient VALUES (?, ?, ?) ON CONFLICT DO NOTHING""")(HPS.set(patient))

        IO.unit
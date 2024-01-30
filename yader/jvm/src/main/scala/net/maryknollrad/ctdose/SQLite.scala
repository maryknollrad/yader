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

case class SQLite() extends DB:
    val xa = Transactor.fromDriverManager[IO](
        "org.sqlite.JDBC", "jdbc:sqlite:yader.db", None
    )

    def datetimeType: String = "DATETIME"
    def now: String = "datetime('now', 'localtime')"
    val minStudyDateAsString: String = "min(studydate)"
    def serial: String = "INTEGER PRIMARY KEY"
    def age(f1: String, f2: String) = s"CAST(SUBSTR(TIMEDIFF($f1, $f2),2,4) as integer)"
    
    def subtime(value: String): Seq[Fragment] = 
        val years = Range(0, 3).map(_ => Some(s"strftime('%Y', $value)")) :+ None
        val rest = Seq(("j", 3), ("W", 2), ("m", 2), ("Y", 4))
                        .map(t => s"format('%0${t._2}d', strftime('%${t._1}', $value))")
        years.zip(rest).map(_ match 
                case (Some(s1), s2) => Fragment.const(s"$s1 || $s2")
                case (None, s2) => Fragment.const(s2))        

    def timeconstraint(value: String) = 
        Seq(1, 7).map(i => fr"date($value, -$i days)")

    def getNow() = 
        sql"SELECT datetime('now', 'localtime')".query[String].unique.transact(xa)
    
    inline def daysbeforetoday(n: Int): String = 
        s"date('now', '-$n days')"
        
    def timeFrags(interval: QueryInterval, from: Int, to: Int): (Fragment, Fragment) = 
        val (fFrag, tFrag) = interval match
            case QueryInterval.Day => 
                (daysbeforetoday(from), daysbeforetoday(to))
            case QueryInterval.Week => 
                import java.time.format.DateTimeFormatter.ISO_DATE
                val (d1, d2) = DB.getWeekBoundary(from, to)
                (s"date('${d1.format(ISO_DATE)}')", s"date('${d2.format(ISO_DATE)}')")
            case QueryInterval.Month => 
                (s"date('now', 'start of month', '-$from months')", 
                    s"date('now', 'start of month', '${-to+1} months', '-1 day')")
            case QueryInterval.Year => 
                (s"date('now', 'start of year', '-$from years')",
                    s"date('now', 'start of year', '${-to+1} years', '-1 day')")
        (Fragment.const(fFrag), Fragment.const(tFrag))

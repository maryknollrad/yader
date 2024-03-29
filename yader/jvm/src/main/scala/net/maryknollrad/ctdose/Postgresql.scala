package net.maryknollrad.ctdose

import doobie.*
import doobie.implicits.*
import cats.effect.IO
import DB.*

case class Postgresql(database: String, user: String, password: String) extends DB:
    val xa: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
        driver = "org.postgresql.Driver", 
        url = s"jdbc:postgresql:$database", 
        user = user, password = password, logHandler = None
    )

    def datetimeType: String = "TIMESTAMP"
    def now: String = "current_timestamp"
    val minStudyDateAsString: String = "to_char(min(studydate), 'YYYY-MM-DD')"
    def serial: String = "SERIAL PRIMARY KEY"
    def age(f1: String, f2: String) = s"EXTRACT(YEAR FROM age($f1::timestamp, $f2::timestamp))"

    def subtime(value: String): Seq[Fragment] = 
        val years = Range(0, 3).map(_ => Some(s"extract(year from $value)")) :+ None
        val rest = Seq(("doy", 3), ("week", 2), ("month", 2), ("year", 4))
                        .map(t => s"lpad(extract(${t._1} from $value)::text, ${t._2}, '0')")
        years.zip(rest).map(_ match 
                case (Some(s1), s2) => Fragment.const(s"concat($s1, $s2)")
                case (None, s2) => Fragment.const(s2))

    inline def daysbeforetoday(n: Int): String = 
        s"date_subtract(current_timestamp, '$n days')::date"

    import doobie.postgres.implicits.*
    def timeFrags(interval: QueryInterval, from: Int, to: Int): (Fragment, Fragment) =         
        interval match
            case QueryInterval.Day => 
                (Fragment.const(daysbeforetoday(from)), Fragment.const(daysbeforetoday(to)))
            case QueryInterval.Week => 
                val (d1, d2) = DB.getWeekBoundary(from, to)
                (fr"$d1", fr"$d2")
            case QueryInterval.Month => 
                (Fragment.const(s"date_trunc('month', current_date) - interval '$from months'"), 
                    Fragment.const(s"date_trunc('month', current_date) + interval '${to + 1} months' - interval '1 day'"))
            case QueryInterval.Year => 
                (Fragment.const(s"date_trunc('year', current_date) - interval '$from years'"), 
                    Fragment.const(s"date_trunc('year', current_date) + interval '${to + 1} years' - interval '1 day'"))

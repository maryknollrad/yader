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
        Seq("day", "week", "month", "year").map(t => Fragment.const(s"extract($t from $value)"))

    inline def daysbeforetoday(n: Int): String = 
        s"date_subtract(current_timestamp, '$n days')::date"

    def timeFrags(interval: QueryInterval, from: Int, to: Int): (Fragment, Fragment) =         
        val (f1str, f2str): Tuple2[String, String] = interval match
            case QueryInterval.Day => 
                (daysbeforetoday(from), daysbeforetoday(to))
            case QueryInterval.Week => 
                (daysbeforetoday(from * 7), daysbeforetoday(to * 7))
            case QueryInterval.Month => 
                (s"date_trunc('month', current_date) - interval '$from months'", 
                    s"date_trunc('month', current_date) + interval '${to + 1} months' - interval '1 day'")
            case QueryInterval.Year => 
                (s"date_trunc('year', current_date) - interval '$from years'", 
                    s"date_trunc('month', current_date) + interval '${to + 1} months' - interval '1 day'")
        (Fragment.const(f1str), Fragment.const(f2str))


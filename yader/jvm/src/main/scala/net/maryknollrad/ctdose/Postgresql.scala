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

    def daysbeforetoday(n: Int): String = 
        s"date_subtract(current_timestamp, '$n days')::date"
package net.maryknollrad.ctdose

import doobie.*
import doobie.implicits.*
import cats.effect.IO
import DB.*

case class Postgresql(database: String, user: String, password: String) extends DB:
    val xa: Transactor.Aux[IO, Unit] = ???
    val now: String = "current_timestamp"
    def intervals(value: String): Seq[Fragment] = 
        Seq("doy", "week", "month", "year").map(t => Fragment.const(s"extract($t from $value)"))

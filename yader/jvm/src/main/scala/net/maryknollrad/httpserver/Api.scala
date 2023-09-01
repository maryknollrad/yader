package net.maryknollrad.httpserver

import cats.effect.*
import org.http4s.* 
import org.http4s.dsl.io.*
import net.maryknollrad.ctdose.DB.{QueryInterval, QueryPartition}
import net.maryknollrad.ctdose.SQLite
import doobie.*, doobie.implicits.* 
import cats.syntax.all.* 
import upickle.default.{ReadWriter => RW, macroRW, write}

object Api:
    private def optionInterval(argument: String): Option[QueryInterval] =
        QueryInterval.values.find(_.strValue == argument)    
    
    private def optionPartition(argument: String): Option[QueryPartition] =
        QueryPartition.values.find(_.strValue == argument)

    private def pAndI[A](partition: String, interval: String)(f:(QueryPartition, QueryInterval) => A): Option[A] = 
        for 
            p <- optionPartition(partition)
            i <- optionInterval(interval)
        yield f(p, i)

    private def bpartsCounts(qi: QueryInterval) = 
        // if selected interval is day, show yesterday else this interval
        val (from, to) = if qi == QueryInterval.Day then (-1, 0) else (0, 0)
        SQLite.getBodypartCounts(qi, from, to).map(ts =>
            val (cats, counts) = ts.foldLeft((Seq.empty[String], Seq.empty[Long]))({ 
                case ((cats, counts), t) => (cats :+ t._1, counts :+ t._3)
            })
            // println(s"$cats, $counts")
            (cats, counts)
        )

    private def doseBox(qi: QueryInterval) = 
        SQLite.partitionedQuery(QueryPartition.Bodypart, qi).to[List].transact(SQLite.xa)
            .map(rs => Data.toBoxedMap(rs))

    val apiService = HttpRoutes.of[IO] {
        case GET -> Root / "echo" / words =>
            Ok(words)

        case GET -> Root / "graphdata" =>
            val qi = Contents.selectedInterval()
            for 
                (cats, counts)  <-  bpartsCounts(qi)
                map             <-  doseBox(qi)
            yield Ok("result")

        case GET -> Root / "bpartsdonut"  =>
            case class Result(categories: Seq[String], counts: Seq[Long]) derives RW
            val qi = Contents.selectedInterval()
            bpartsCounts(qi).flatMap((cats, counts) => Ok(write(Result(cats, counts))))

        // json data for ApexChart boxplot
        case GET -> Root / "boxdata" / partition / interval =>
            pAndI(partition, interval)((p, i) => 
                SQLite.partitionedQuery(p, i)
                    .to[List].transact(SQLite.xa)
                    .flatMap(rs => 
                        val retouched = Data.toBoxedMap(rs)
                        Ok(write(retouched)))).getOrElse(BadRequest())

        // json data when user selects category from above boxplot
        case GET -> Root / "boxdetail" / partition / partitionValue / interval =>
            pAndI(partition, interval)((p, i) =>
                SQLite.partitionedQuery(p, i, Some(partitionValue))
                    .to[List].transact(SQLite.xa)
                    .flatMap(rs =>
                        val retouched = Data.toBoxedMapWithDetails(rs)
                        Ok(write(retouched)))).getOrElse(BadRequest())

        // case GET -> Root/ "patientdose" / chartId => ???
    }

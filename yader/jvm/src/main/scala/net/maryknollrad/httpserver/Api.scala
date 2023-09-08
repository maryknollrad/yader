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
    def optionInterval(argument: String): Option[QueryInterval] =
        QueryInterval.values.find(_.strValue == argument)    
    
    def optionPartition(argument: String): Option[QueryPartition] =
        QueryPartition.values.find(_.strValue == argument)

    def pAndI[A](partition: String, interval: String)(f:(QueryPartition, QueryInterval) => A): Option[A] = 
        for 
            p <- optionPartition(partition)
            i <- optionInterval(interval)
        yield f(p, i)

    private def bpartsCounts(qi: QueryInterval, from: Int, to: Int) = 
        SQLite.getBodypartCounts(qi, from, to).map(ts =>
            val (cats, counts) = ts.foldLeft((Seq.empty[String], Seq.empty[Long]))({ 
                case ((cats, counts), t) => (cats :+ t._1, counts :+ t._3)
            })
            (cats, counts)
        )

    private def doseBox(qi: QueryInterval, from: Int, to: Int) = 
        SQLite.partitionedQuery(QueryPartition.Bodypart, qi, from = from, to = to)
            .map(rs => Data.toBoxedMap(rs))

    val apiService = HttpRoutes.of[IO] {
        case GET -> Root / "echo" / words =>
            println(words)
            Ok(words)

        case GET -> Root / "graphdata" / IntVar(i) if i >= 0 && i <= QueryInterval.qiSize =>
            case class GraphData(bodyparts: Seq[String], bodypartsCounts: Seq[Long], 
                bodypartBox: Map[String, Map[String, Seq[Double]]]) derives RW
            val qi = QueryInterval.fromOrdinal(i)
            // if selected interval is day, show yesterday else this interval
            val (from, to) = QueryInterval.defaultRange(qi)
            bpartsCounts(qi, from, to).flatMap(bparts =>
                doseBox(qi, from, to).flatMap(boxmap => 
                    Ok(write(GraphData(bparts._1, bparts._2, boxmap)))
                ))

        // json data for ApexChart boxplot
        /*
        case GET -> Root / "boxdata" / partition / interval =>
            pAndI(partition, interval)((p, i) => 
                SQLite.partitionedQuery(p, i)
                    .flatMap(rs => 
                        val retouched = Data.toBoxedMap(rs)
                        Ok(write(retouched)))).getOrElse(BadRequest())
        */

        // json data when user clicks boxplot
        /*
        case GET -> Root / "boxdetail" / partition / partitionValue / interval =>
            pAndI(partition, interval)((p, i) =>
                SQLite.partitionedQuery(p, i, Some(partitionValue))
                    .flatMap(rs =>
                        val retouched = Data.toBoxedMapWithDetails(rs)
                        Ok(write(retouched)))).getOrElse(BadRequest())
        */
        // case GET -> Root/ "patientdose" / chartId => ???
    }

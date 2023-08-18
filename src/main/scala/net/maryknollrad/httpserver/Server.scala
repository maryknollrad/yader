package net.maryknollrad.httpserver

import cats.effect.*
import org.http4s.* 
import org.http4s.dsl.io.*
import com.comcast.ip4s.*
import org.http4s.ember.server.*
import org.http4s.server.Router
import org.http4s.implicits.*
import org.http4s.headers.{Origin, `Content-Type`}
import fs2.io.file.Path

object HttpServer:
    private val index = """<!DOCTYPE html>
    |<html lang="en">
    |  <head>
    |    <meta charset="UTF-8">
    |    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    |    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    |    <title>Yet Another Dose Extractor and Reporter</title>
    |    <link rel="stylesheet" href="./style.css">
    |    <link rel="icon" href="./favicon.ico" type="image/x-icon">
    |  </head>
    |  <body>
    |    <main>
    |        <h1>YADER with Lamina</h1> 
    |    </main>
    |    <div id="app"></div>
    |	<script src="index.js"></script>
    |  </body>
    |</html>""".stripMargin
    // <script src="https://cdn.jsdelivr.net/npm/apexcharts"></script>

    private val textHtmlType = MediaType.unsafeParse("text/html")

    import net.maryknollrad.ctdose.DB.{QueryInterval, QueryPartition}

    private def optionInterval(argument: String): Option[QueryInterval] =
        QueryInterval.values.find(_.strValue == argument)
    
    private def optionPartition(argument: String): Option[QueryPartition] =
        QueryPartition.values.find(_.strValue == argument)

    import net.maryknollrad.ctdose.SQLite
    import doobie.*, doobie.implicits.* 
    import cats.syntax.all.* 
    import upickle.default.{ReadWriter => RW, macroRW, write}
    import org.http4s.server.middleware.*

    private def pAndI[A](partition: String, interval: String)(f:(QueryPartition, QueryInterval) => A): Option[A] = 
        for 
            p <- optionPartition(partition)
            i <- optionInterval(interval)
        yield f(p, i)

    val yaderService = HttpRoutes.of[IO] {
        case GET -> Root =>
            Ok(index, `Content-Type`(textHtmlType))

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
        
        case request@GET -> Root / "index.js" =>
            // StaticFile.fromath(Path("/Users/nineclue/lab/yader-f/target/scala-3.3.0/yaderf-fastopt/main.js"), Some(request)).getOrElseF(NotFound())
            StaticFile.fromPath(Path("/Users/nineclue/lab/yader-f/target/scala-3.3.0/scalajs-bundler/main/yaderf-fastopt-bundle.js"), Some(request)).getOrElseF(NotFound())
    }

    /*
    val service = CORS.policy.withAllowOriginHost(Set(
            Origin.Host(Uri.Scheme.http, Uri.RegName("localhost"), Some(5000)),
        ))(yaderService)
    */
    val corsService = CORS.policy.withAllowOriginAll(yaderService)

    val server = EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"7878")
        .withHttpApp(corsService.orNotFound)
        .build
        .use(_ => IO.never)
package net.maryknollrad.httpserver

import cats.effect.*
import org.http4s.* 
import org.http4s.dsl.io.*
import com.comcast.ip4s.*
import org.http4s.ember.server.*
import org.http4s.server.Router
import org.http4s.implicits.*
import org.http4s.headers.`Content-Type`
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

    val helloWorldService = HttpRoutes.of[IO] {
        case GET -> Root =>
            Ok(index, `Content-Type`(MediaType.unsafeParse("text/html")))
        case request@GET -> Root / "index.js" =>
            // StaticFile.fromath(Path("/Users/nineclue/lab/yader-f/target/scala-3.3.0/yaderf-fastopt/main.js"), Some(request)).getOrElseF(NotFound())
            StaticFile.fromPath(Path("/Users/nineclue/lab/yader-f/target/scala-3.3.0/scalajs-bundler/main/yaderf-fastopt-bundle.js"), Some(request)).getOrElseF(NotFound())
    }

    val server = EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"7878")
        .withHttpApp(helloWorldService.orNotFound)
        .build
        .use(_ => IO.never)
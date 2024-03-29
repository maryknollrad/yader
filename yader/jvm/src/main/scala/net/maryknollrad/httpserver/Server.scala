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
import net.maryknollrad.ctdose.Configuration.CTDoseConfig
import net.maryknollrad.ctdose.CTDose

object HttpServer:
    import org.http4s.server.middleware.*
    import org.slf4j.{Logger, LoggerFactory}
    private val logger = LoggerFactory.getLogger(getClass)
    private val textHtmlType = MediaType.unsafeParse("text/html")

    val staticService = HttpRoutes.of[IO] {
        case GET -> Root =>
            Ok(Contents.index, `Content-Type`(textHtmlType))

        // case request@GET -> Root / "yader.js" =>
            // StaticFile.fromPath(Path("./yader/js/target/scala-3.3.1/yader-fastopt/main.js"), Some(request)).getOrElseF(NotFound())
            // StaticFile.fromPath(Path("./yader/js/target/scala-3.3.1/yader-opt/main.js"), Some(request)).getOrElseF(NotFound())

        case request@GET -> Root / "assets" / file =>
            StaticFile.fromResource(file, Some(request)).getOrElseF(NotFound())
    }

    def server(cdi: CTDoseConfig) = 
        val portNum = cdi.webPort.getOrElse(7878)
        val api = Api(cdi)
        val contents = Contents(cdi)
        val yaderService = Router("/" -> staticService, "/api" -> api.apiService, "/c" -> contents.contentService)
        /*
        val service = CORS.policy.withAllowOriginHost(Set(
                Origin.Host(Uri.Scheme.http, Uri.RegName("localhost"), Some(5000)),
            ))(yaderService)
        */
        val corsService = CORS.policy.withAllowOriginAll(yaderService)
        EmberServerBuilder
            .default[IO]
            .withHost(ipv4"0.0.0.0")
            .withPort(Port.fromInt(portNum).get)
            .withHttpApp(corsService.orNotFound)
            .build
            .use(_ => 
                IO.print(s"Running server at port: $portNum\n")
                *> IO.never
            )
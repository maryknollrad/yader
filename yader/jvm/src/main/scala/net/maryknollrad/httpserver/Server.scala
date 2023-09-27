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

    private val textHtmlType = MediaType.unsafeParse("text/html")

    val staticService = HttpRoutes.of[IO] {
        case GET -> Root =>
            Ok(Contents.index, `Content-Type`(textHtmlType))

        case request@GET -> Root / "yader.js" =>
            StaticFile.fromPath(Path("./yader/js/target/scala-3.3.0/yader-fastopt/main.js"), Some(request)).getOrElseF(NotFound())

        case request@GET -> Root / "assets" / file =>
            StaticFile.fromResource(file, Some(request)).getOrElseF(NotFound())
    }

    def server(cdi: CTDoseConfig) = 
        val portNum = cdi.webPort.getOrElse(7878)
        val yaderService = Router("/" -> staticService, "/api" -> Api.apiService, "/c" -> Contents.contentService(cdi.institution.head, cdi.doseDLP))
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
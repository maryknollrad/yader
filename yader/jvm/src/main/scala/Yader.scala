import net.maryknollrad.ctdose.{Configuration, CTDoseInfo}
import cats.effect.*
import net.maryknollrad.httpserver.HttpServer
import cats.syntax.all.*

object Yader extends IOApp:
    def run(as: List[String]): IO[ExitCode] = 
        // "trace", "debug", "info", "warn", "error" or "off"
        val logProperty = "org.slf4j.simpleLogger.defaultLogLevel"
        Option(System.getProperty(logProperty)).getOrElse(System.setProperty(logProperty, "error"))

        Configuration.loadConfigAndRun({
            case (cdi, cti) => 
                (CTDoseInfo.run(cdi, cti), HttpServer.server(cdi)).parMapN { (_, _) => () }
                // HttpServer.server(cdi)
        })
        // Configuration.loadConfigAndRun({ case (cdi, cti) => DRLVals.initCategories(cdi.db) })        
        *> IO(ExitCode.Success)
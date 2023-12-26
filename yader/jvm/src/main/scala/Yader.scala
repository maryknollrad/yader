import net.maryknollrad.d4cs.{CFind, CGet, DicomBase, RetrieveLevel, DicomTags}
import net.maryknollrad.ctdose.{Configuration, Tesseract, CTDose, CTDoseInfo, CTDRL, DB}
import net.maryknollrad.ctdose.SQLite

import DicomBase.*
import java.time.{LocalDate, LocalTime, LocalDateTime}
import java.time.format.DateTimeFormatter
import org.dcm4che3.data.Tag
import RetrieveLevel.*
import DicomBase.*
import cats.effect.*
import org.dcm4che3.data.Attributes
import Configuration.ConnectionInfo
import java.awt.image.BufferedImage
import CTDose.DoseResultRaw
import CTDoseInfo.*
import org.dcm4che3.data.ElementDictionary
import net.maryknollrad.httpserver.HttpServer
import cats.syntax.all.*
import net.maryknollrad.ctdose.DRLVals

object Yader extends IOApp:
    def run(as: List[String]): IO[ExitCode] = 
        // "trace", "debug", "info", "warn", "error" or "off"
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "error")

        Configuration.loadConfigAndRun({
            case (cdi, cti) => 
                (CTDoseInfo.run(cdi, cti), HttpServer.server(cdi)).parMapN { (_, _) => () }
                // HttpServer.server(cdi)
        })
        // Configuration.loadConfigAndRun({ case (cdi, cti) => DRLVals.initCategories(cdi.db) })        
        *> IO(ExitCode.Success)
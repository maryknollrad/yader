package net.maryknollrad.ctdose

import org.ekrich.config.* 
import java.io.File
import scala.util.Try

object Configuration:
    case class ConnectionInfo(callingAe: String, calledAe: String, host: String, port: Int, encoding: String) 
    type TesseractPath = String

    def apply(fname: String = "dicom"): Either[String, (ConnectionInfo,  TesseractPath)] =
        val ifile = File(s"$fname.conf")
        if ifile.exists() then 
            Try:
                val c = ConfigFactory.parseFile(ifile)
                val ci = ConnectionInfo(
                    c.getString("calling-ae"),
                    c.getString("called-ae"),
                    c.getString("host"),
                    c.getInt("port"),
                    c.getString("encoding")
                )
                val tpath = c.getString("tesseract-path")
                (ci, tpath)
            .toEither.left.map(_.getMessage())
        else Left(s"Cannot find $fname.conf")
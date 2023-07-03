import org.ekrich.config.* 
import java.io.File
import scala.util.Try

object Configuration:
    case class ConnectionInfo(callingAe: String, calledAe: String, host: String, port: Int, encoding: String) 

    def apply(fname: String = "dicom"): Either[String, ConnectionInfo] =
        val ifile = File(s"$fname.conf")
        if ifile.exists() then 
            Try:
                val c = ConfigFactory.parseFile(ifile)
                ConnectionInfo(
                    c.getString("calling-ae"),
                    c.getString("called-ae"),
                    c.getString("host"),
                    c.getInt("port"),
                    c.getString("encoding")
                )
            .toEither.left.map(_.getMessage())
        else Left(s"Cannot find $fname.conf")
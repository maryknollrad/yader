import cats.effect.*
import cats.syntax.all.*

object Console:
    def nonEmpty(s: String) = if s.trim().isEmpty then Some("Answer should not be empty") else None

    private val ip = raw"(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})".r
    def isIPAddr(s: String) = s match
        case ip(a, b, c, d) if a.toInt <= 255 && b.toInt <= 255 && c.toInt <= 255 && d.toInt <= 255 => None
        case _ => Some(s"IP address is not in correct format : $s")

    def isNumber(s: String) = scala.util.Try(s.toInt).toOption match
        case Some(_) => None
        case _ => Some("Number should be given")

    def numberInRange(r: Range)(s: String) = scala.util.Try(s.toInt).toOption match
        case Some(n) if r.contains(n) => None
        case _ => Some(s"Number should be given and in range between ${r.start} and ${r.end+1}")

    def lengthInRange(r: Range)(s: String) = 
        if r.contains(s.length) then None
        else Some(s"String too long. Should be less than or equal to ${r.end}")

    def printAndRead(msg: String, validateFunction: Option[String => Option[String]] = None,
                default: Option[String] = None): IO[String] = 
        val defaultAppended = default.map(d => s"$msg [$d] : ").getOrElse(msg ++ " : ")
        val h = 
            for 
                _   <- IO.print(defaultAppended) 
                ans <- IO.readLine
            yield ans
        def r: IO[String] = h.flatMap(s => 
            val amended = if default.nonEmpty && s.trim().isEmpty() then default.get else s
            validateFunction.map(f => f(amended) match
                case None => IO.pure(amended)
                case Some(msg) => IO.println(msg) *> r
            ).getOrElse(IO.pure(amended)))
        r
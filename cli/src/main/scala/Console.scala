import cats.effect.*
import cats.syntax.all.*

object Console:
    type ConsValidate = String => Either[String, String]

    def nonEmpty(s: String) = if s.isEmpty then Left("Answer should not be empty") else Right(s)

    private val ip = raw"(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})".r
    def isIPAddr(s: String) = s match
        case ip(a, b, c, d) if a.toInt <= 255 && b.toInt <= 255 && c.toInt <= 255 && d.toInt <= 255 => Right(s)
        case _ => Left(s"IP address is not in correct format : $s")

    def isNumber(s: String) = scala.util.Try(s.toInt).toOption match
        case Some(_) => Right(s)
        case _ => Left("Number should be given")

    def numberInRange(r: Range)(s: String) = scala.util.Try(s.toInt).toOption match
        case Some(n) if r.contains(n) => Right(s)
        case _ => Left(s"Number should be given and in range between ${r.start} and ${r.end+1}")

    def lengthInRange(r: Range)(s: String) = 
        if r.contains(s.length) then Right(s)
        else Left(s"String too long or short. Should be in range between ${r.start} and${r.end}")

    def oneOf(choices: Seq[String]) = 
        val lowCs = choices.withFilter(_.trim().nonEmpty).map(_.toLowerCase())
        val cSample = lowCs.map(c => 
            val rest = if c.tail.isEmpty then "" else s"[${c.tail}]"
            s"${c.head}$rest")
        assert(cSample.length > 1)
        val choiceSample = cSample.init.mkString(",") ++ s" or ${cSample.last}"
        (s: String) =>
            nonEmpty(s).flatMap(ss => 
                val lowS = ss.toLowerCase()
                lowCs.find(c => c == lowS || c.head == lowS.head) match
                    case Some(_) => Right(s)
                    case _ => Left(s"Please enter $choiceSample"))

    def yesOrNo = oneOf(Seq("yes", "no"))

    def printAndRead(msg: String, validateFunction: Option[ConsValidate] = None,
                default: Option[String] = None): IO[String] = 
        val defaultAppended = default.map(d => s"$msg [$d] : ").getOrElse(msg ++ " : ")
        val h = 
            for 
                _   <- IO.print(defaultAppended) 
                ans <- IO.readLine
            yield ans
        def r: IO[String] = h.flatMap(s => 
            val amended = default match
                case Some(d) if s.trim().isEmpty() => d
                case _ => s.trim()
            validateFunction.map(f => f(amended) match
                case Right(s) => IO.pure(s)
                case Left(msg) => IO.println(msg) *> r
            ).getOrElse(IO.pure(amended)))
        r
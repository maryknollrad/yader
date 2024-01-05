import cats.effect.*

object YaderCli extends IOApp:
    def run(as: List[String]): IO[ExitCode] = 
        IO.println("Hello, world") *> IO(ExitCode.Success)
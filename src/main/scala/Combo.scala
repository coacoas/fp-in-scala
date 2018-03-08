import scala.language.higherKinds

import cats.data.EitherK
import cats.free.Free

object Combo {

  type ConsoleLogo[A] = EitherK[Console, Logo, A]
  def program(implicit C: ConsoleInject[ConsoleLogo], L: LogoInject[ConsoleLogo]): Free[ConsoleLogo, Unit] = {
    import C._, L._

    val p0 = Position(30, 30)
    for {
      _  <- writeLine(s"Starting at: $p0")
      p1 <- up(p0, 10)
      _  <- writeLine(s"Position: $p1")
      p2 <- down(p1, 15)
      _  <- writeLine(s"Position: $p2")
      p3 <- left(p2, 34)
      _  <- writeLine(s"Position: $p3")
      p4 <- right(p3, 18)
      _  <- writeLine(s"Position: $p4")
    } yield ()
  }

  def _main(args: Array[String]) = {
    val interpreter = ConsoleIO or LogoIO
    program.foldMap(interpreter).unsafeRunSync
  }
}

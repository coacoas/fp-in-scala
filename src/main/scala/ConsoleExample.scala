import scala.language.higherKinds

import cats.{~>, InjectK}
import cats.effect.IO
import cats.free.Free

sealed trait Console[A]
final case object ReadLine extends Console[String]
final case class WriteLine(s: String) extends Console[Unit]

object ConsoleApp {
  val op = ConsoleInject.console[Console]
  import op._

  val program: Free[Console, Unit] = for {
    _    <- writeLine(s"Please enter your name:")
    name <- readLine
    _    <- writeLine(s"Hello, $name!")
  } yield ()

  def _main(args: Array[String]) = program.foldMap(ConsoleIO).unsafeRunSync()
}

object ConsoleIO extends (Console ~> IO) {
  def apply[A](c: Console[A]): IO[A] = c match {
    case ReadLine => IO { readLine() }
    case WriteLine(s) => IO { println(s) }
  }
}

class ConsoleInject[F[_]](implicit I: InjectK[Console, F]) {
  def readLine: Free[F, String] = Free.inject[Console, F](ReadLine)
  def writeLine(s: String): Free[F, Unit] = Free.inject[Console, F](WriteLine(s))
}

object ConsoleInject {
  implicit def console[F[_]](implicit I: InjectK[Console, F]): ConsoleInject[F] = {
    new ConsoleInject[F]
  }
}

object ConsoleOp {
  def readLine: Free[Console, String] = Free.liftF(ReadLine)
  def writeLine(s: String): Free[Console, Unit] = Free.liftF(WriteLine(s))
}

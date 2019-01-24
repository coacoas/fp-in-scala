import scala.language.higherKinds

import cats.free.Free
import cats.InjectK
import cats.effect.IO
import cats.{~>}

sealed trait Console[A]
case object ReadLine extends Console[String]
case class WriteLine(s: String) extends Console[Unit]

class ConsoleInject[F[_]](implicit I: InjectK[Console, F]) {
  def readLine: Free[F, String] = Free.inject(ReadLine)
  def writeLine(s: String): Free[F, Unit] = Free.inject(WriteLine(s))
}
object ConsoleInject {
  implicit def logo[F[_]](implicit I: InjectK[Console, F]): ConsoleInject[F] = {
    new ConsoleInject[F]
  }

  val op = logo[Console]
}

object ConsoleApp {
  import ConsoleInject.op._

  val program: Free[Console, Unit] = for {
    _    <- writeLine(s"Please enter your name:")
    name <- readLine
    _    <- writeLine(s"Hello, $name!")
  } yield ()
}

object ConsoleIO extends (Console ~> IO) {
  def apply[A](c: Console[A]): IO[A] = c match {
    case ReadLine => IO { readLine() }
    case WriteLine(s) => IO { println(s) }
  }
}

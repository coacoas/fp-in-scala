import scala.language.higherKinds
import cats.effect.IO
import cats.free.Free
import cats.InjectK
import cats.{~>}

case class Position(x: Int, y: Int)

sealed trait Logo[A]
case class Up(p: Position, distance: Int) extends Logo[Position]
case class Down(p: Position, distance: Int) extends Logo[Position]
case class Left(p: Position, distance: Int) extends Logo[Position]
case class Right(p: Position, distance: Int) extends Logo[Position]

class LogoInject[F[_]](implicit I: InjectK[Logo, F]) {
  def up(p: Position, distance: Int): Free[F, Position] = Free.inject[Logo, F](Up(p, distance))
  def down(p: Position, distance: Int): Free[F, Position] = Free.inject[Logo, F](Down(p, distance))
  def left(p: Position, distance: Int): Free[F, Position] = Free.inject[Logo, F](Left(p, distance))
  def right(p: Position, distance: Int): Free[F, Position] = Free.inject[Logo, F](Right(p, distance))
}

object LogoInject {
  implicit def logo[F[_]](implicit I: InjectK[Logo, F]): LogoInject[F] = {
    new LogoInject[F]
  }

  val op = logo[Logo]
}

object LogoIO extends (Logo ~> IO) {
  def apply[A](c: Logo[A]): IO[A] = c match {
    case Up(p: Position, distance: Int) => IO.pure(p.copy(y = p.y + distance))
    case Down(p: Position, distance: Int) => IO.pure(p.copy(y = p.y - distance))
    case Left(p: Position, distance: Int) => IO.pure(p.copy(x = p.x + distance))
    case Right(p: Position, distance: Int) => IO.pure(p.copy(x = p.x - distance))
  }
}

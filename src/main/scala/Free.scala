package example

import scala.language.higherKinds

import cats.Monad
import cats.{~>}

sealed trait Free[F[_], A] {
  import Free.{Pure, Suspend, Bind}

  def map[B](f: A => B): Free[F, B] = flatMap(a => Free.pure(f(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Free.flatMap(this)(f)

  def foldMap[G[_] : Monad](nt: F ~> G): G[A] = this match {
    case Pure(a) => Monad[G].pure(a)
    case Suspend(fa) => nt.apply(fa)
    case Bind(target, f) =>
      Monad[G].flatMap(target.foldMap(nt)) { e =>
        f(e).foldMap(nt)
      }
  }
}

object Free {
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)

  def flatMap[F[_], E, A](fa: Free[F, E])(f: E => Free[F, A]): Free[F, A] =
    Bind(fa, f)

  case class Pure[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
  case class Bind[F[_], E, A](target: Free[F, E], f: E => Free[F, A]) extends Free[F, A]
}

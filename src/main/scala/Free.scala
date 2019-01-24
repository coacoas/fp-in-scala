import cats.Monad
import cats.{~>}

import scala.language.higherKinds

sealed trait Free_[F[_], A] {
  import Free_._
  def foldMap[G[_]: Monad](nt: F ~> G): G[A] =
    this match {
      case Pure(a) => Monad[G].pure(a)
      case Suspend(fa) => nt.apply(fa)
      case Bind(target, f) =>
        Monad[G].flatMap(
          target.foldMap(nt)
        ) { a =>
          f(a).foldMap(nt)
        }
    }
}

object Free_  {
  def pure[F[_], A](a: A): Free_[F, A] =
    Pure(a)

  def liftF[F[_], A](fa: F[A]): Free_[F, A] =
    Suspend(fa)

  def flatMap[F[_], A, B](
    fa: Free_[F, A]
  )(
    f: A => Free_[F, B]
  ): Free_[F, B] = Bind(fa, f)

  case class Pure[F[_], A](a: A) extends Free_[F, A]
  case class Suspend[F[_], A](fa: F[A]) extends Free_[F, A]
  case class Bind[F[_], E, A](
    target: Free_[F, E],
    f: E => Free_[F, A]) extends Free_[F, A]
}

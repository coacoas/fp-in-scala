import language.higherKinds

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]
  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
}

object Monad {
  import Functor.Instances._
  import Applicative.Instances._

  def apply[F[_] : Monad] = implicitly[Monad[F]]

  implicit val optionMonad = new Monad[Option] {
    // Members declared in Applicative
    def ap[A, B](a: Option[A],b: Option[A => B]): Option[B] = Applicative[Option].ap(a, b)
    
    // Members declared in Functor
    def map[A, B](a: Option[A],f: A => B): Option[B] = Functor[Option].map(a, f)
    
    // Members declared in Monad
    def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = a match {
      case None => None
      case Some(x) => f(x)
    }

    def >>=[A, B](a: Option[A])(f: A => Option[B]): Option[B] = flatMap(a)(f)
  }
}

import language.higherKinds

trait Applicative[F[_]] extends Functor[F] { 
  def ap[A, B](a: F[A], b: F[A => B]): F[B]

  def map2[A, B, C](
    a: F[A], 
    b: F[B], 
    c: (A, B) => C): F[C] = 
    ap(b, map(a, c.curried))
}

object Applicative {
  def apply[F[_] : Applicative] = implicitly[Applicative[F]]

  object Instances {
    import Functor.Instances._
    implicit val optionApplicative = new Applicative[Option] {
      def ap[A, B](a: Option[A], f: Option[A => B]): Option[B] = a match {
        case None => None
        case Some(x) => f match {
          case None => None
          case Some(g) => Option(g(x))
        }
      }

      def map[A, B](a: Option[A], f: A => B): Option[B] = Functor[Option].map(a, f)
    }
  }
}

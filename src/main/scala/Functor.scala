import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](a: F[A], f: A => B): F[B]
}

object Functor {
  def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]

  object Instances {
    implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      def map[A, B](a: Option[A], f: A => B): Option[B] = a match {
        case Some(x) => Option(f(x))
        case None => None
      }
    }

    implicit val listFunctor: Functor[List] = new Functor[List] {
      def map[A, B](as: List[A], f: A => B): List[B] = {
        val applied  = as.foldLeft(List.empty[B]) { (bs, a) => f(a) :: bs }
        applied.reverse
      }
    }
  }
}

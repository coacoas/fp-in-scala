trait Monoid[A] {
  val zero: A
  def append(a: A, b: A): A
}

object Monoid {
  def apply[A : Monoid]: Monoid[A] = implicitly[Monoid[A]]

  object Instances {
    implicit val stringConcat = new Monoid[String] {
      val zero = ""
      def append(a: String, b: String) = a + b
    }

    implicit val integerAddition = new Monoid[Int] {
      val zero = 0
      def append(a: Int, b: Int) = a + b
    }

    val integerMultiplication = new Monoid[Int] {
      val zero = 1
      def append(a: Int, b: Int) = a * b
    }

    def optionMonoid[A : Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
      val zero = None
      def append(o1: Option[A], o2: Option[A]): Option[A] = o1 match {
        case None => o2
        case Some(x) => o2 match {
          case None => o1
          case Some(y) => Some(Monoid[A].append(x, y))
        }
      }
    }
  }
}

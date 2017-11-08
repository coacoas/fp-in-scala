trait Monoid[A] {
  val zero: A
  def append(a: A, b: A): A
}

object Monoid[A] {
  def apply: Monoid[A] = implicitly[Monoid[A]]
}

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
}

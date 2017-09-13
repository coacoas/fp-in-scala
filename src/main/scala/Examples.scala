object Examples {
  def divisibleBy(k: Int): Int => Boolean = i => i % k == 0
  val isEven: Int => Boolean = divisibleBy(2)

  type Predicate[A] = A => Boolean

  def not[A](p: Predicate[A]): Predicate[A] = a => !p(a)
  val isOdd: Predicate[Int] = not(isEven)

  def lift[A](f: (Boolean, Boolean) => Boolean):
      (Predicate[A], Predicate[A]) => Predicate[A] =
    (p1, p2) => a => f(p1(a), p2(a))

  def and[A] = lift[A](_ && _)
  def or[A] = lift[A](_ || _)

  def sum1(xs: List[Int]): Int =
    xs match {
      case Nil => 0
      case y :: ys => y + sum1(ys)
    }

  def foldRight[A, B](
    xs: List[A], z: B
  )(
    f: (A, B) => B
  ): B =
    xs match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B) = {
    @annotation.tailrec
    def loop(acc: B, remaining: List[A]): B =
      remaining match {
        case Nil => acc
        case x :: xs => loop(f(acc, x), xs)
      }

    loop(z, xs)
  }

  def sum(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)
  def product(xs: List[Int]): Int = foldLeft(xs, 1)(_ * _)
  def mkString(xs: List[Int]): String = foldLeft(xs, "")(_ + _.toString)
  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, List.empty[A]) { (acc, i) => i :: acc }

  def map[A, B](xs: List[A])(f: A => B): List[B] = foldRight(xs, List.empty[B]) { (i, acc) => f(i) :: acc }
  def mapl[A, B](xs: List[A])(f: A => B): List[B] =
    reverse(foldLeft(xs, List.empty[B]) { (acc, i) => f(i) :: acc })
}

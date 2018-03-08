import cats.~>
import cats.data.State
import org.scalatest.{ FlatSpec, Matchers }

class ConsoleTest extends FlatSpec with Matchers {
  "Console" should "read and write" in {
    val in = List("Jax.Ex")
    val out = Vector.empty[String]
    val expected = Vector(
      "Please enter your name:",
      "Hello, Jax.Ex!"
    )

    val (io, _) = ConsoleApp.program.foldMap(ConsoleState).run(InOut(in, out)).value
    io.out should equal(expected)
  }
}

case class InOut(in: List[String], out: Vector[String])
object ConsoleState extends (Console ~> State[InOut, ?]) {
  def apply[A](c: Console[A]): State[InOut, A] =
    c match {
      case ReadLine => State {
        case InOut(in, out) => InOut(in.tail, out) -> in.head
      }
      case WriteLine(s) => State.modify {
        case InOut(in, out) => InOut(in, out :+ s)
      }
    }
}

package turbolift.operations
import turbolift.abstraction._
import turbolift.std_effects._
import org.specs2._


class WriterTest extends Specification {
  def is = {
    case object Fx extends Writer[Vector[Int]]

    (for {
      _ <- Fx.tell(1)
      _ <- Fx.tell(2) *! Fx.tell(3) *! Fx.tell(4)
      _ <- Fx.tell(5)
    } yield ())
    .runWith(Fx.handler.justState) must_== (1 to 5)
  }
}

package turbolift.operations
import turbolift.abstraction.!!
import turbolift.abstraction.implicits._
import turbolift.std_effects.State
import org.specs2._


class StateTests extends Specification {
  def is = {
    case object Fx extends State[Int]

    (for {
      a <- Fx.get
      _ <- Fx.put(a + 99)
      _ <- Fx.put(a + 10)
      b <- Fx.get
      _ <- Fx.put(a + 999)
      _ <- Fx.put(a + 100)
      c <- Fx.get
      _ <- Fx.put(a + 9999)
      _ <- Fx.put(c + 1000)
    } yield ())
    .runWith(Fx.handler(1).exec) must_== 1101
  }
}

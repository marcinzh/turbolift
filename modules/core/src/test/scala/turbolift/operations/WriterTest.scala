package turbolift.operations
import turbolift.abstraction.!!
import turbolift.std_effects.Writer
import org.specs2._


class WriterTest extends Specification {
  def is = List(tell, listen, censor).reduce(_ ^ _)

  def tell = br ^ "tell" ! {
    case object Fx extends Writer[Vector[Int]]

    (for {
      _ <- Fx.tell(1)
      _ <- Fx.tell(2) *! Fx.tell(3) *! Fx.tell(4)
      _ <- Fx.tell(5)
    } yield ())
    .runWith(Fx.handler.justState) must_== (1 to 5)
  }

  def listen = br ^ "listen" ! {
    case object Fx extends Writer[Vector[Int]]

    (for {
      _ <- Fx.tell(1)
      xx <- Fx.listen (for {
        _ <- Fx.tell(2)
        _ <- Fx.tell(3)
      } yield ())
      _ <- Fx.tell(4)
    } yield xx._1)
    .runWith(Fx.handler) must_== (1 to 4, 2 to 3)
  }

  def censor = br ^ "censor" ! {
    case object Fx extends Writer[Vector[Int]]

    (for {
      _ <- Fx.tell(1)
      _ <- Fx.censor (for {
        _ <- Fx.tell(2)
        _ <- Fx.tell(3)
      } yield ()) (_.map(_ * -1))
      _ <- Fx.tell(4)
    } yield ())
    .runWith(Fx.handler.justState) must_== Vector(1, -2, -3, 4)
  }
}

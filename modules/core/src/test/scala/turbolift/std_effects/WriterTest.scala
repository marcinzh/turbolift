package turbolift.operations
import turbolift.abstraction.!!
import turbolift.std_effects.{Writer, WriterK, WriterGK}
import cats.implicits._
import org.specs2._


class WriterTest extends Specification {
  def is = List(tell, listen, censor, writerG).reduce(_ ^ _)

  def tell = br ^ "tell" ! {
    case object Fx extends Writer[Int]

    val comp = for {
      _ <- Fx.tell(1)
      _ <- Fx.tell(2) *! Fx.tell(3) *! Fx.tell(4)
      _ <- Fx.tell(5)
    } yield ()

    comp.runWith(Fx.handler.justState) must_== 15
  }

  def listen = br ^ "listen" ! {
    case object Fx extends WriterK[Vector, Int]

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
    case object Fx extends WriterK[Vector, Int]

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

  def writerG = br ^ "WriterG" ! {
    case object Fx extends WriterGK[Map, Vector, String, Int]
    (for {
      _ <- Fx.tell("a", 1)
      _ <- Fx.tell("b", 10)
      _ <- Fx.tell("a", 2)
      _ <- Fx.tell("b", 20)
      _ <- Fx.tell("c", 100)
      _ <- Fx.tell("c", 200)
      _ <- Fx.tell("b", 30)
      _ <- Fx.tell("c", 300)
      _ <- Fx.tell("a", 3)
    } yield ())
    .runWith(Fx.handler.justState) must_== Map(
      "a" -> Vector(1, 2, 3),
      "b" -> Vector(10, 20, 30),
      "c" -> Vector(100, 200, 300),
    )
  }
}

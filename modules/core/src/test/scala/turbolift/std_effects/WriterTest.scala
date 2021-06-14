package turbolift.std_effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!
import turbolift.std_effects.{Writer, WriterK, WriterGK}


class WriterTest extends AnyFlatSpec:
  "tell" should "work" in {
    case object Fx extends Writer[Int]

    val comp = for
      _ <- Fx.tell(1)
      _ <- Fx.tell(2) *! Fx.tell(3) *! Fx.tell(4)
      _ <- Fx.tell(5)
    yield ()

    comp.runWith(Fx.handler.justState) shouldEqual 15
  }

  "listen" should "work" in {
    case object Fx extends WriterK[Vector, Int]

    (for
      _ <- Fx.tell(1)
      xx <- Fx.listen (for
        _ <- Fx.tell(2)
        _ <- Fx.tell(3)
      yield ())
      _ <- Fx.tell(4)
    yield xx._1)
    .runWith(Fx.handler) shouldEqual (1 to 4, 2 to 3)
  }

  "censor" should "work" in {
    case object Fx extends WriterK[Vector, Int]

    (for
      _ <- Fx.tell(1)
      _ <- Fx.censor (for
        _ <- Fx.tell(2)
        _ <- Fx.tell(3)
      yield ()) (_.map(_ * -1))
      _ <- Fx.tell(4)
    yield ())
    .runWith(Fx.handler.justState) shouldEqual Vector(1, -2, -3, 4)
  }

  "WriterGK" should "work" in {
    case object Fx extends WriterGK[Map, String, Vector, Int]
    (for
      _ <- Fx.tell("a", 1)
      _ <- Fx.tell("b", 10)
      _ <- Fx.tell("a", 2)
      _ <- Fx.tell("b", 20)
      _ <- Fx.tell("c", 100)
      _ <- Fx.tell("c", 200)
      _ <- Fx.tell("b", 30)
      _ <- Fx.tell("c", 300)
      _ <- Fx.tell("a", 3)
    yield ())
    .runWith(Fx.handler.justState) shouldEqual Map(
      "a" -> Vector(1, 2, 3),
      "b" -> Vector(10, 20, 30),
      "c" -> Vector(100, 200, 300),
    )
  }

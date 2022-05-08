package turbolift.std_effects
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.!!
import turbolift.std_effects.{Writer, WriterK, WriterGK}


class WriterTest extends AnyFunSpec:
  describe("Basic ops") {
    it("tell") {
      case object Fx extends Writer[Int]
      Fx.tell(1)
      .runWith(Fx.handler) shouldEqual (((), 1))
    }

    it("listen") {
      case object Fx extends Writer[Int]
      Fx.listen(Fx.tell(1))
      .runWith(Fx.handler) shouldEqual ((((), 1), 1))
    }

    it("censor") {
      case object Fx extends Writer[Int]
      Fx.censor(_ + 1)(Fx.tell(1))
      .runWith(Fx.handler) shouldEqual (((), 2))
    }
  }

  describe("Combined ops") {
    it("tell") {
      case object Fx extends Writer[String]
      (Fx.tell("a") &&! Fx.tell("b"))
      .runWith(Fx.handler.justState) shouldEqual "ab"
    }

    it("tell & listen") {
      case object Fx extends Writer[String]
      (for
        _ <- Fx.tell("a")
        workaround <- Fx.listen (Fx.tell("b") &&! Fx.tell("c"))
        ((), x) = workaround
        _ <- Fx.tell("d")
      yield x)
      .runWith(Fx.handler) shouldEqual ("bc", "abcd")
    }
  }

  describe("Par ops") {
    it("tell") {
      case object Fx extends Writer[String]
      (Fx.tell("a") &&! (Fx.tell("b") *! Fx.tell("c")) &&! Fx.tell("d"))
      .runWith(Fx.handler.justState) shouldEqual "abcd"
    }

    it("tell & censor") {
      case object Fx extends Writer[String]
      (Fx.tell("a") &&!
      Fx.censor(x => s"($x)") {
        Fx.tell("b") *!
        Fx.censor(x => s"[$x]") { Fx.tell("c") } *!
        Fx.tell("d") *!
        Fx.censor(x => s"{$x}") { Fx.tell("e") } *!
        Fx.tell("f")
      } &&!
      Fx.tell("g"))
      .runWith(Fx.handler.justState) shouldEqual "a(b[c]d{e}f)g"
    }
  }

  describe("Into collections") {
    it("WriterK") {
      case object Fx extends WriterK[Vector, Char]
      (for
        _ <- Fx.tell('a')
        _ <- Fx.tells("bc".toVector)
        _ <- Fx.tell('d')
      yield ())
      .runWith(Fx.handler.justState) shouldEqual "abcd".toVector
    }

    it("WriterGK") {
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
  }

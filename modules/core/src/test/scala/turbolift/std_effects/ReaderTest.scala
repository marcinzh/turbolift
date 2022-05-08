package turbolift.std_effects
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.!!
import turbolift.std_effects.{Reader, WriterK}


class ReaderTest extends AnyFunSpec:
  describe("Basic ops") {
    it("ask") {
      case object Fx extends Reader[Int]
      Fx.ask
      .runWith(Fx.handler(1)) shouldEqual 1
    }

    it("asks") {
      case object Fx extends Reader[(Int, Int)]
      Fx.asks(_._1)
      .runWith(Fx.handler((1, 2))) shouldEqual 1
    }

    it("localPut") {
      case object Fx extends Reader[Int]
      Fx.localPut(2) { Fx.ask }
      .runWith(Fx.handler(1)) shouldEqual 2
    }
  }


  describe("Combined ops") {
    it("ask & localPut") {
      case object Fx extends Reader[Int]
      (Fx.localPut(2) { Fx.ask } **! Fx.ask)
      .runWith(Fx.handler(1)) shouldEqual ((2, 1))
    }

    it("Reader & Writer") {
      case object FxR extends Reader[Int]
      case object FxW extends WriterK[Vector, String]

      def loop(str: String): Unit !! FxR.type with FxW.type =
        if str.isEmpty then
          !!.pure()
        else 
          str.head match
            case '[' => FxR.localModify(_ + 1)(loop(str.tail))
            case ']' => FxR.localModify(_ - 1)(loop(str.tail))
            case x =>
              for
                indent <- FxR.ask
                _ <- FxW.tell(("  " * indent) :+ x)
                _ <- loop(str.tail) 
              yield ()

      val lines1 = 
        loop("ab[cd[e]f[]g]h")
        .runWith(FxW.handler.justState &&&! FxR.handler(0))
        .mkString("\n")

      val lines2 = """
        |a
        |b
        |  c
        |  d
        |    e
        |  f
        |  g
        |h
        |""".stripMargin.tail.init

      lines1 shouldEqual lines2
    }
  }


  describe("Par ops") {
    it("ask & localModify") {
      case object Fx extends Reader[Int]
      Fx.localModify(_ + 1) {
        Fx.ask *!
        Fx.localModify(_ + 10) { Fx.ask } *!
        Fx.ask *!
        Fx.localModify(_ + 100) { Fx.ask } *!
        Fx.ask
      }
      .map { case ((((a, b), c), d), e) => (a, b, c, d, e) }
      .runWith(Fx.handler(1)) shouldEqual ((2, 12, 2, 102, 2))
    }
  }

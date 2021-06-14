package turbolift.std_effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!
import turbolift.std_effects.{Reader, WriterK}


class ReaderTest extends AnyFlatSpec:
  "ask" should "work" in {
    case object Fx extends Reader[Int]

    Fx.ask.runWith(Fx.handler(42)) shouldEqual 42
  }

  "asks" should "work" in {
    type Env = (Int, String)
    case object Fx extends Reader[Env]

    (for
      i <- Fx.asks(_._1)
      s <- Fx.asks(_._2)
    yield (i, s))
    .runWith(Fx.handler((42, "foo"))) shouldEqual (42, "foo")
  }

  "local" should "work" in {
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
      .runWith(FxR.handler(0) <<<! FxW.handler.justState)
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

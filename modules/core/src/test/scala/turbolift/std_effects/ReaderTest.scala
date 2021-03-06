package turbolift.operations
import cats.implicits._
import turbolift.abstraction.!!
import turbolift.std_effects.{Reader, WriterK}
import org.specs2._


class ReaderTest extends Specification {

  def is = List(ask, asks, local).reduce(_ ^ _)

  def ask = br ^ "ask" ! {
    case object Fx extends Reader[Int]

    Fx.ask.runWith(Fx.handler(42)) must_== 42
  }

  def asks = br ^ "asks" ! {
    type Env = (Int, String)
    case object Fx extends Reader[Env]

    (for {
      i <- Fx.asks(_._1)
      s <- Fx.asks(_._2)
    } yield (i, s))
    .runWith(Fx.handler((42, "foo"))) must_== (42, "foo")
  }

  def local = br ^ "local" ! {
    case object FxR extends Reader[Int]
    case object FxW extends WriterK[Vector, String]

    def loop(str: String): Unit !! FxR.type with FxW.type = {
      if (str.isEmpty)
        !!.pure()
      else 
        str.head match {
          case '[' => FxR.localModify(_ + 1)(loop(str.tail))
          case ']' => FxR.localModify(_ - 1)(loop(str.tail))
          case x => for { 
            indent <- FxR.ask
            _ <- FxW.tell(("  " * indent) :+ x)
            _ <- loop(str.tail) 
          } yield ()
        }
    }

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

    lines1 must_== lines2
  }
}

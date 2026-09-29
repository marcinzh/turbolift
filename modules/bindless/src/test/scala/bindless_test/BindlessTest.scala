package turbolift.effects.bindless_test
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects._
import turbolift.bindless._


object MacroSafeSpace:
  def basic =
    case object S extends StateEffect[Int]
    case object W extends WriterEffect[String]
    case object R extends ReaderEffect[Boolean]

    `do`:
      val s = S.getModify(_ * 10).!
      W.tell("omg").!
      if R.ask.! then
        W.tell(" it works").!
      s + 1
    .handleWith(S.handler(42))
    .handleWith(W.handler)
    .handleWith(R.handler(true))
    .run


  def nested =
    case object R extends ReaderEffect[Int]
    case object S extends StateEffect[Int]
    `do`:
      S.put(R.ask.!).!
    .handleWith(R.handler(1337))
    .handleWith(S.handler(42))
    .run


class BindlessTest extends Specification:
  "basic" >>{
    MacroSafeSpace.basic.===((43, 420), "omg it works")
  }

  "nested" >>{
    MacroSafeSpace.nested.===((), 1337)
  }

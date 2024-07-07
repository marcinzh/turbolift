package turbolift.effects.bindless_test
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects._
import turbolift.bindless._


object MacroSafeSpace:
  def stuff =
    case object S extends State[Int]
    case object W extends Writer[String]
    case object R extends Reader[Boolean]

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


class BindlessTest extends Specification:
  "test" >>{
    MacroSafeSpace.stuff.===((43, 420), "omg it works")
  }

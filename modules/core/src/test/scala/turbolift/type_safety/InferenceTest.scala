package turbolift.type_safety
import org.specs2._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import turbolift.!!
import turbolift.effects.{ReaderEffect, WriterEffect, StateEffect, ChoiceEffect}
import turbolift.mode.ST


class InferenceTest extends Specification:
  def is = br ^ "Effect inference" ! stuff

  def stuff =
    case object S extends StateEffect[Double]
    case object W extends WriterEffect[String]
    case object R extends ReaderEffect[Boolean]
    case object C extends ChoiceEffect
    type S = S.type
    type W = W.type
    type R = R.type
    type C = C.type

    val prog =
      for
        _ <- !!.unit
        workaround <- S.get *! R.ask
        (a, b) = workaround
        _ <- W.tell("lies")
        _ <- C.choose(1 to 10)
        // if c % 3 == 0
      yield ()

    type Expected = Unit !! (S & W & R & C)

    Typecheck {"implicitly[prog.type <:< Expected]"} must succeed

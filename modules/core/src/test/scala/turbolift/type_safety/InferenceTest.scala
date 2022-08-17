package turbolift.type_safety
import org.specs2._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import turbolift.!!
import turbolift.std_effects.{Reader, Writer, State, Choice}
import turbolift.mode.ST


class InferenceTest extends Specification:
  def is = br ^ "Effect inference" ! stuff

  def stuff =
    case object S extends State[Double]
    case object W extends Writer[String]
    case object R extends Reader[Boolean]
    case object C extends Choice
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

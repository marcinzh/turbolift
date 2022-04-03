package turbolift.type_safety
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.!!
import turbolift.std_effects.{Reader, Writer, State, Choice}


class InferenceTest extends AnyFunSpec:
  describe("Inference of effects types") {
    case object Fx1 extends State[Double]
    case object Fx2 extends Writer[String]
    case object Fx3 extends Reader[Boolean]
    case object Fx4 extends Choice

    val comp = for
      _ <- !!.pure()
      workaround <- Fx1.get *! Fx3.ask
      (a, b) = workaround
      _ <- Fx2.tell("lies")
      _ <- Fx4.choose(1 to 10)
      // if c % 3 == 0
    yield ()

    type Expected = Unit !! Fx1.type with Fx2.type with Fx3.type with Fx4.type

    assertCompiles {"implicitly[comp.type <:< Expected]"}
  }

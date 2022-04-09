package turbolift.stack_safety
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.std_effects.Choice


class WideTest extends AnyFunSpec with CanStackOverflow:
  describe("Choice from big collection should be stack safe") {
    case object Fx extends Choice

    val comp = for
      i <- Fx.choose(1 to TooBigForStack)
    yield i
    
    mustNotStackOverflow {
      comp.runWith(Fx.handler)
    }
  }


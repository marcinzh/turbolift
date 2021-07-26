package turbolift.stack_safety
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.std_effects.Choice


class WideTest extends AnyFlatSpec with CanStackOverflow:
  "Choice from big collection" should "be stack safe" in {
    case object Fx extends Choice

    val comp = for
      i <- Fx.each(1 to TooBigForStack)
    yield i
    
    mustNotStackOverflow {
      comp.runWith(Fx.handler)
    }
  }


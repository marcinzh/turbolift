package turbolift.stack_safety
import turbolift.abstraction._
import turbolift.std_effects._
import org.specs2._


class WideTest extends Specification with CanStackOverflow {
  def is = choice

  def choice = br ^ "Choice from big collection should be stack safe" ! mustNotStackOverflow {
    case object Fx extends NonDet

    (for {
      i <- Fx.each(1 to TooBigForStack)
    } yield i)
    .runWith(Fx.handler)
  }
}
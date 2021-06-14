package turbolift.std_effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!
import turbolift.std_effects.{Choice, Except}


class ChoiceTest extends AnyFlatSpec with CanLaunchTheMissiles:
  "each: without guard" should "work" in {
    case object Fx extends Choice

    val comp = for {
      i <- Fx.each(1 to 2)
      c <- Fx.each('a' to 'b')
    } yield s"$i$c"

    comp.runWith(Fx.handler) shouldEqual Vector("1a", "1b", "2a", "2b")
  }


  "each: with guard" should "work" in {
    case object Fx extends Choice

    val comp = for {
      i <- Fx.each(0 to 3)
      if i % 2 != 0
      c <- Fx.each('a' to 'c')
    } yield s"$i$c"

    comp.runWith(Fx.handler) shouldEqual Vector("1a", "1b", "1c", "3a", "3b", "3c")
  }


  "Choice with Except" should "work" in {
    case object FxC extends Choice
    case object FxE extends Except[Int]

    val comp = !!.pure(1) ||! FxE.raise(2)

    comp.runWith(FxC.handler <<<! FxE.handler) shouldEqual Vector(Right(1), Left(2))
    comp.runWith(FxE.handler <<<! FxC.handler) shouldEqual Left(2)
  }


  "fail" should "work" in {
    case object Fx extends Choice

    val missile1 = Missile()
    val missile2 = Missile()

    (for
      i <- !!.pure(123)
      _ <- Fx.fail *! missile1.launch_!
      _ <- missile2.launch_!
    yield i)
    .runWith(Fx.handlers.one) shouldEqual None
    
    missile1.mustHaveLaunchedOnce
    missile2.mustNotHaveLaunched
  }

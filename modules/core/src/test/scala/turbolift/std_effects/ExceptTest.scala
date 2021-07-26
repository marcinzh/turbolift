package turbolift.std_effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!
import turbolift.std_effects.{Except, State}


class ExceptTest extends AnyFlatSpec with CanLaunchTheMissiles:

  "raise" should "work" in {
    case object Fx extends Except[String]

    def mkEff =
      val missile1 = Missile()
      val missile2 = Missile()
      val missile3 = Missile()
      val comp = 
        for
          i <- !!.pure(123)
          _ <- missile1.launch_! *! Fx.raise("turn") *! missile2.launch_!
          _ <- missile3.launch_!
        yield i
      (comp, missile1, missile2, missile3)
    
    val (comp, missile1, missile2, missile3) = mkEff
    comp.runWith(Fx.handler) shouldEqual Left("turn")
    missile1.mustHaveLaunchedOnce
    missile2.mustHaveLaunchedOnce
    missile3.mustNotHaveLaunched
  }

  "katch" should "work" in {
    case object FxE extends Except[String]
    case object FxS extends State[Int]

    def mkEff =
      val missile = Missile()
      val comp = FxE.katch {
        for
          _ <- FxS.put(42)
          _ <- FxE.raise("OMG")
          _ <- FxS.put(1337)
          _ <- missile.launch_!
        yield true
      } (_ => !!.pure(false))
      (comp, missile)

    {
      val (comp, missile) = mkEff
      val result = comp.runWith(FxE.handler <<<! FxS.handler(0))
      result shouldEqual Right((0, false))
      missile.mustNotHaveLaunched
    }

    {
      val (comp, missile) = mkEff
      val result = comp.runWith(FxS.handler(0) <<<! FxE.handler)
      result shouldEqual ((42, Right(false)))
      missile.mustNotHaveLaunched
    }

    // br ^ "katch: State <<<! Except" ! testSE ^
    // br ^ "katch: Except <<<! State" ! testES
  }

package turbolift.operations
import turbolift.abstraction.!!
import turbolift.std_effects.{Except, State}
import org.specs2._


class ExceptTest extends Specification with CanLaunchTheMissiles {
  def is = List(raise, katch).reduce(_ ^ _)

  def raise = br ^ "raise" ! {
    case object Fx extends Except[String]

    def mkEff = {
      val missile1 = Missile()
      val missile2 = Missile()
      val missile3 = Missile()
      val comp = 
        for {
          i <- !!.pure(123)
          _ <- missile1.launch_! *! Fx.raise("turn") *! missile2.launch_!
          _ <- missile3.launch_!
        } yield i
      (comp, missile1, missile2, missile3)
    }
    
    val (comp, missile1, missile2, missile3) = mkEff
    comp.runWith(Fx.handler) must_== Left("turn") and 
    missile1.mustHaveLaunchedOnce and
    missile2.mustHaveLaunchedOnce and
    missile3.mustNotHaveLaunched
  }

  def katch = {
    case object FxE extends Except[String]
    case object FxS extends State[Int]

    def mkEff = {
      val missile = Missile()
      val comp = FxE.katch {
        for {
          _ <- FxS.put(42)
          _ <- FxE.raise("OMG")
          _ <- FxS.put(1337)
          _ <- missile.launch_!
        } yield true
      } (_ => !!.pure(false))
      (comp, missile)
    }

    val testES = {
      val (comp, missile) = mkEff
      val result = comp.runWith(FxE.handler <<<! FxS.handler(0))
      result must_== Right((0, false)) and
      missile.mustNotHaveLaunched
    }

    val testSE = {
      val (comp, missile) = mkEff
      val result = comp.runWith(FxS.handler(0) <<<! FxE.handler)
      result must_== ((42, Right(false))) and
      missile.mustNotHaveLaunched
    }

    br ^ "katch: State <<<! Except" ! testSE ^
    br ^ "katch: Except <<<! State" ! testES
  }
}

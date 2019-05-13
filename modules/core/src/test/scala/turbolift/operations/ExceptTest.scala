package turbolift.operations
import turbolift.abstraction._
import turbolift.std_effects._
import org.specs2._


class ExceptTest extends Specification with CanLaunchTheMissiles {
  def is = {
    case object Fx extends Except[String]

    def mkEff = {
      val missile1 = Missile()
      val missile2 = Missile()
      val eff = 
        for {
          i <- Return(123)
          _ <- Fx.raise("turn") *! missile1.launch_!
          _ <- missile2.launch_!
        } yield i
      (eff, missile1, missile2)
    }
    
    val (eff, missile1, missile2) = mkEff
    eff.runWith(Fx.handler) must_== Left("turn") and 
    missile1.mustHaveLaunchedOnce and
    missile2.mustNotHaveLaunched
  }
}

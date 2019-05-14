package turbolift.operations
import turbolift.abstraction._
import turbolift.std_effects._
import org.specs2._


class MaybeTest extends Specification with CanLaunchTheMissiles {
  def is = {
    case object Fx extends Maybe

    val missile1 = Missile()
    val missile2 = Missile()
    (for {
      i <- Return(123)
      _ <- Fx.fail *! missile1.launch_!
      _ <- missile2.launch_!
    } yield i)
    .runWith(Fx.handler) must_== None and 
    missile1.mustHaveLaunchedOnce and
    missile2.mustNotHaveLaunched
  }
}

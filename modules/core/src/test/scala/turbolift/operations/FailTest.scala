package turbolift.operations
import turbolift.abstraction.!!
import turbolift.abstraction.implicits._
import turbolift.std_effects.Fail
import org.specs2._


class FailTest extends Specification with CanLaunchTheMissiles {
  def is = {
    case object Fx extends Fail

    val missile1 = Missile()
    val missile2 = Missile()
    (for {
      i <- !!.pure(123)
      _ <- Fx.fail *! missile1.launch_!
      _ <- missile2.launch_!
    } yield i)
    .runWith(Fx.handler) must_== None and 
    missile1.mustHaveLaunchedOnce and
    missile2.mustNotHaveLaunched
  }
}

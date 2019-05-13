package turbolift.operations
import turbolift.abstraction._
import turbolift.std_effects._
import org.specs2._


class ValidationTest extends Specification with CanLaunchTheMissiles {
  def is = {
    case object Fx extends Validation[String]

    val missile1 = Missile()
    val missile2 = Missile()
    (for {
      _ <- Fx.invalid("x") *! missile1.launch_! *! Fx.invalid("y") *! Fx.invalid("z")
      _ <- missile2.launch_!
      _ <- Fx.invalid("w")
    } yield ())
    .runWith(Fx.handler) must_== Left("xyz") and
    missile1.mustHaveLaunchedOnce and
    missile2.mustNotHaveLaunched
  }
}

package turbolift.effects
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.MaybeEffect
import turbolift.mode.ST


class MaybeTest extends Specification with CanLaunchTheMissiles:
  case object M extends MaybeEffect

  "Basic ops" >> {
    "some" >>{
      !!.pure(42)
      .handleWith(M.handler)
      .run === Some(42)
    }

    "none" >>{
      val missile = Missile()
      (M.empty &&! missile.launch_!)
      .handleWith(M.handler)
      .run === None

      missile.mustNotHaveLaunched
    }

    "catchToOption some" >>{
      M.catchToOption(!!.pure(42))
      .handleWith(M.handler)
      .run === Some(Some(42))
    }

    "catchToOption none" >>{
      val missile = Missile()
      M.catchToOption(M.empty &&! missile.launch_!)
      .handleWith(M.handler)
      .run === Some(None)

      missile.mustNotHaveLaunched
    }
  }

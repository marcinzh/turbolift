package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, ErrorEffect}
import turbolift.effects.CanLaunchTheMissiles
import turbolift.data.{Outcome, Cause}
import turbolift.mode.ST


class InspectTest extends Specification with CanLaunchTheMissiles:
  "Basic ops" >> {
    "guarantee & success" >>{
      val missile = Missile()
      IO.guarantee(missile.launch_!):
        !!.pure(1)
      .runIO === Outcome.Success(1)
      missile.mustHaveLaunchedOnce
    }

    "guarantee & throw" >>{
      val missile = Missile()
      val e = new Exception("OMG")
      IO.guarantee(missile.launch_!):
        IO(throw e)
      .runIO === Outcome.Failure(Cause(e))
      missile.mustHaveLaunchedOnce
    }

    "guarantee & raise" >>{
      val missile = Missile()
      val e = new Exception("OMG")
      IO.guarantee(missile.launch_!):
        IO.raise(e)
      .runIO === Outcome.Failure(Cause(e))
      missile.mustHaveLaunchedOnce
    }

    "guarantee & self-cancel" >>{
      val missile = Missile()
      IO.guarantee(missile.launch_!):
        IO.cancel
      .runIO == Outcome.Cancelled
      missile.mustHaveLaunchedOnce
    }
  }

  "Combined ops" >> {
    "guarantee & error" >>{
      case object E extends ErrorEffect[String]
      val missile = Missile()
      IO.guarantee(missile.launch_!):
        E.raise("OMG")
      .as(())
      .handleWith(E.handler)
      .runIO === Outcome.Success(Left("OMG"))
      missile.mustHaveLaunchedOnce
    }
  }

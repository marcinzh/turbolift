package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, ErrorEffect}
import turbolift.effects.CanLaunchTheMissiles
import turbolift.data.{Outcome, Cause}
import turbolift.runtime.ST


class InspectTest extends Specification with CanLaunchTheMissiles:
  "Basic ops" >> {
    "guarantee & success" >>{
      val missile = Missile()
      IO.guarantee(missile.launch_!):
        !!.pure(1)
      .runSync === Outcome.Success(1)
      missile.mustHaveLaunchedOnce
    }

    "guarantee & throw" >>{
      val missile = Missile()
      val e = new Exception("OMG")
      IO.guarantee(missile.launch_!):
        IO(throw e)
      .runSync === Outcome.Failure(Cause(e))
      missile.mustHaveLaunchedOnce
    }

    "guarantee & raise" >>{
      val missile = Missile()
      val e = new Exception("OMG")
      IO.guarantee(missile.launch_!):
        IO.raise(e)
      .runSync === Outcome.Failure(Cause(e))
      missile.mustHaveLaunchedOnce
    }

    "guarantee & self-cancel" >>{
      val missile = Missile()
      IO.guarantee(missile.launch_!):
        IO.cancel
      .runSync === Outcome.Cancelled
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
      .runSync === Outcome.Success(Left("OMG"))
      missile.mustHaveLaunchedOnce
    }
  }

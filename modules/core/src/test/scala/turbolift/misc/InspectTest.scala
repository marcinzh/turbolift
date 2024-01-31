package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, Error}
import turbolift.effects.CanLaunchTheMissiles
import turbolift.io.{Outcome, Cause}
import turbolift.mode.ST


class InspectTest extends Specification with CanLaunchTheMissiles:
  "Basic ops" >> {
    "guarantee & success" >>{
      val missile = Missile()
      IO.guarantee(missile.launch_!):
        !!.pure(1)
      .unsafeRun === Outcome.Success(1)
      missile.mustHaveLaunchedOnce
    }

    "guarantee & throw" >>{
      val missile = Missile()
      val e = new Exception("OMG")
      IO.guarantee(missile.launch_!):
        !!.impure(throw e)
      .unsafeRun === Outcome.Failure(Cause(e))
      missile.mustHaveLaunchedOnce
    }

    "guarantee & fail" >>{
      val missile = Missile()
      val e = new Exception("OMG")
      IO.guarantee(missile.launch_!):
        IO.fail(e)
      .unsafeRun === Outcome.Failure(Cause(e))
      missile.mustHaveLaunchedOnce
    }

    "guarantee & self-cancel" >>{
      val missile = Missile()
      IO.guarantee(missile.launch_!):
        IO.cancel
      .unsafeRun == Outcome.Cancelled
      missile.mustHaveLaunchedOnce
    }
  }


  "Combined ops" >> {
    "guarantee & fail" >>{
      case object E extends Error[String]
      val missile = Missile()
      IO.guarantee(missile.launch_!):
        E.raise("OMG")
      .as(())
      .handleWith(E.handler)
      .unsafeRun === Outcome.Success(Left("OMG"))
      missile.mustHaveLaunchedOnce
    }
  }

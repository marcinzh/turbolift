package turbolift.std_effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!


trait CanLaunchTheMissiles:
  this: AnyFlatSpec =>

  case class Missile():
    private var count = 0 
    def launch() = { count += 1 }
    def launch_! = !!.eval(launch())
    def launchedOnce = count == 1
    def mustHaveLaunchedOnce = assert(count == 1)
    def mustNotHaveLaunched = assert(count == 0)

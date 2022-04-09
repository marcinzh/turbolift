package turbolift.std_effects
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.!!


trait CanLaunchTheMissiles:
  this: AnyFunSpec =>

  case class Missile():
    private var count = 0 
    def launch() = { count += 1 }
    def launch_! = !!.impure(launch())
    def launchedOnce = count == 1
    def mustHaveLaunchedOnce = assert(count == 1)
    def mustNotHaveLaunched = assert(count == 0)

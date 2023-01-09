package turbolift.extra_effects
import org.specs2.mutable._
import turbolift.!!


trait CanLaunchTheMissiles:
  this: Specification =>

  case class Missile():
    private var count = 0 
    def launch() = { count += 1 }
    def launch_! = !!.impure(launch())
    def launchCount = count
    def launchedOnce = count == 1
    def mustHaveLaunchedOnce = count === 1
    def mustNotHaveLaunched = count === 0

  object Missile:
    def make(n: Int): Vector[Missile] = Vector.fill(n)(Missile())

package turbolift.operations
import turbolift.abstraction.!!
import org.specs2._


trait CanLaunchTheMissiles { this: Specification => 
  case class Missile() { 
    private var count = 0 
    def launch() = { count += 1 }
    def launch_! = !!.eval(launch())
    def launchedOnce = count == 1
    def mustHaveLaunchedOnce = count must_== 1
    def mustNotHaveLaunched = count must_== 0
  }
}

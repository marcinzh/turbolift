package turbolift.internals.launcher
import scala.util.Try
import turbolift.Computation
import turbolift.effects.IO
import turbolift.internals.executor.{ZeroThreadedExecutor, MultiThreadedExecutor}
import turbolift.internals.engine.Config


final case class Launcher(
   multiThreaded: Boolean,
   //@#@TODO
):
  def run[A](comp: Computation[A, IO]): Try[A] =
    val exec =
      if multiThreaded
      then MultiThreadedExecutor.default
      else new ZeroThreadedExecutor
    val config = Config.default(executor = exec)
    exec.run(comp, config)


object Launcher:
  def default: Launcher = MT

  val MT: Launcher = Launcher(multiThreaded = true)

  val ST: Launcher = Launcher(multiThreaded = false)


package turbolift.internals.launcher
import java.util.concurrent.{Executor, Executors, ExecutorService, ThreadFactory, ForkJoinPool, ForkJoinTask}
import scala.util.Try
import turbolift.!!
import turbolift.io.IO
import turbolift.internals.engine.{Config => EngineConfig, Fiber}


private[internals] object Launcher:
  def run[A](using config: LauncherConfig)(comp: A !! IO): Try[A] =
    if config.multiThreaded
    then multiThreaded(comp)
    else singleThreaded(comp)

  private def singleThreaded[A](comp: A !! IO): Try[A] =
    val exec = new ZeroThreadedExecutor
    val config = EngineConfig.default(executor = exec)
    val promise = Promise.singleThreaded()
    Fiber.makeRoot(comp, promise.untyped, config).submit()
    exec.drainAll()
    promise.get

  private def multiThreaded[A](comp: A !! IO): Try[A] =
    val exec = MultiThreadedExecutor.get()
    val config = EngineConfig.default(executor = exec)
    val promise = Promise.multiThreaded()
    Fiber.makeRoot(comp, promise.untyped, config).submit()
    val result = promise.get
    if promise.panicked then
      MultiThreadedExecutor.kill()
    result

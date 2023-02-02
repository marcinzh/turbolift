package turbolift.internals.launcher
import java.util.concurrent.{ExecutorService, ForkJoinPool}


private[launcher] object MultiThreadedExecutor:
  private var exec: ExecutorService | Null = null
  private def create(): ExecutorService = new ForkJoinPool

  def get() =
    synchronized {
      if exec == null then
        exec = create()
      exec.nn
    }

  def kill() =
    synchronized {
      if exec != null then
        exec.nn.shutdownNow()
        exec = null
    }

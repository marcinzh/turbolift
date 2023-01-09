package turbolift.internals.launcher
import java.util.concurrent.{Executor, Executors, ExecutorService, ThreadFactory, ForkJoinPool, ForkJoinTask}
import scala.util.Try
import turbolift.!!
import turbolift.internals.engine.{Config, Fiber, Panic}


private[internals] sealed abstract class Launcher[F[_]]:
  enclosing =>

  def run[A](comp: A !! ?): F[A]

  final def map[G[_]](f: [X] => F[X] => G[X]): Launcher[G] = new:
    override def run[A](comp: A !! ?): G[A] = f(enclosing.run(comp))


private[internals] object Launcher:
  def default(using config: LauncherConfig): Launcher[[X] =>> X] = sync.unsafeGet

  def sync(using config: LauncherConfig): Launcher[Try] =
    if config.multiThreaded then
      new MultiThreaded
    else
      new SingleThreaded

  extension (thiz: Launcher[Try])
    def unsafeGet: Launcher[[X] =>> X] = thiz.map([X] => (xx: Try[X]) => xx.get)


private class SingleThreaded extends Launcher[Try]:
  override def run[A](comp: A !! ?): Try[A] =
    val exec = new ZeroThreadExecutor
    val callback = new Callback.Sync[A]
    val config = Config.default(executor = exec)
    Fiber.makeRoot(comp, config, callback).submit()
    exec.drainAll()
    callback.get


private final class MultiThreaded extends Launcher[Try]:
  override def run[A](comp: A !! ?): Try[A] =
    val exec = MultiThreaded.get()
    val callback = new Callback.Notify[A]
    val config = Config.default(executor = exec)
    Fiber.makeRoot(comp, config, callback).submit()
    if !ForkJoinTask.inForkJoinPool() then
      callback.await()
    else
      callback.blocking()
    if callback.panicked then
      MultiThreaded.kill()
    callback.get


private object MultiThreaded:
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


private final class ZeroThreadExecutor extends Executor:
  private val que = new java.util.LinkedList[Runnable]

  override def execute(r: Runnable): Unit = que.addLast(r)

  def kill(): Unit = que.clear()

  def drainAll(): Unit =
    while !que.isEmpty do
      que.removeFirst.nn.run()

package turbolift.internals.executor
import java.util.concurrent.{Executors, Executor => JExecutor, ArrayBlockingQueue}
import scala.concurrent.ExecutionContext
import turbolift.Computation
import turbolift.io.Outcome
import turbolift.internals.engine.{FiberImpl, MainLoop, Halt}


private[turbolift] final class ForeignExecutor(val underlying: ExecutionContext) extends Executor:
  private[turbolift] override def resume(fiber: FiberImpl): Unit =
    underlying.execute:
      new MainLoop(fiber):
        override def run(): Unit =
          runCurrent() match
            case Halt.Yield => resume(getCurrentFiber)
            case _ => ()


  override def runSync[A](comp: Computation[A, ?], name: String): Outcome[A] =
    val queue = new ArrayBlockingQueue[Outcome[A]](1)
    runAsync(comp, name, queue.offer)
    queue.take().nn


  override def runAsync[A](comp: Computation[A, ?], name: String, callback: Outcome[A] => Unit): Unit =
    val fiber = FiberImpl.create(comp, this, name, isReentry = false, callback)
    resume(fiber)


private[turbolift] object ForeignExecutor:
  lazy val default = fromJava(Executors.newWorkStealingPool().nn)

  def fromJava(e: JExecutor): ForeignExecutor = new ForeignExecutor(ExecutionContext.fromExecutor(e))
  def fromScala(e: ExecutionContext): ForeignExecutor = new ForeignExecutor(e)

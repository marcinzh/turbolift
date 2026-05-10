package turbolift.internals.executor
import java.util.concurrent.{Executors, Executor => JExecutor, ArrayBlockingQueue}
import scala.concurrent.ExecutionContext
import turbolift.Computation
import turbolift.data.Outcome
import turbolift.runtime.Runtime
import turbolift.internals.engine.FiberImpl


private[turbolift] final case class ForeignExecutor(underlying: ExecutionContext) extends Executor:
  private[turbolift] override def resume(fiber: FiberImpl): Unit =
    underlying.execute(fiber)


  override def runSync[A](comp: Computation[A, ?],  runtime: Runtime, name: String): Outcome[A] =
    val queue = new ArrayBlockingQueue[Outcome[A]](1)
    runAsync(comp, runtime, queue.offer, name)
    queue.take().nn


  override def runAsync[A](comp: Computation[A, ?],  runtime: Runtime, callback: Outcome[A] => Unit, name: String): Unit =
    val fiber = FiberImpl.createRoot(comp, runtime, name, isReentry = false, callback)
    resume(fiber)


private[turbolift] object ForeignExecutor:
  lazy val default = fromJava(Executors.newWorkStealingPool().nn)

  def fromJava(e: JExecutor): ForeignExecutor = ForeignExecutor(ExecutionContext.fromExecutor(e))
  def fromScala(e: ExecutionContext): ForeignExecutor = ForeignExecutor(e)

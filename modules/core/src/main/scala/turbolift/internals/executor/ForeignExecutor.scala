package turbolift.internals.executor
import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.ExecutionContext
import turbolift.Computation
import turbolift.io.Outcome
import turbolift.internals.engine.{FiberImpl, Halt}


final class ForeignExecutor(val underlying: ExecutionContext) extends Executor:
  override def resume(fiber: FiberImpl): Unit =
    underlying.execute:
      new Runnable:
        override def run(): Unit =
          fiber.run() match
            case Halt.Yield(yielder) => resume(yielder)
            case _ => ()


  override def runSync[A](comp: Computation[A, ?], name: String): Outcome[A] =
    val queue = new ArrayBlockingQueue[Outcome[A]](1)
    runAsync(comp, name, queue.offer)
    queue.take().nn


  override def runAsync[A](comp: Computation[A, ?], name: String, callback: Outcome[A] => Unit): Unit =
    val fiber = FiberImpl.create(comp, this, name, isReentry = false, callback)
    resume(fiber)

package turbolift.internals.executor
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.internals.engine.{FiberImpl, Halt, Link}


private[internals] final class ZeroThreadedExecutor extends Link.Queue with Executor:
  override protected def detectReentry(): Boolean = !isEmpty //// Not always correct, but harmless

  override def start(fiber: FiberImpl, isReentry: Boolean): Unit = drain(fiber)

  override def resume(fiber: FiberImpl): Unit = enqueue(fiber)

  @tailrec private def drain(fiber: FiberImpl): Unit =
    fiber.run() match
      case Halt.Yield(yielder) =>
        if isEmpty then
          drain(yielder)
        else
          enqueue(yielder)
          drain(dequeue())

      case _ =>
        if !isEmpty then
          drain(dequeue())

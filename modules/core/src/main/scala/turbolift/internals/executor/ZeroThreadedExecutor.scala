package turbolift.internals.executor
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.internals.engine.{FiberImpl, Link}


private[internals] final class ZeroThreadedExecutor extends Link.Queue with Executor:
  override def detectReentry(): Boolean = !isEmpty //// Not always correct, but harmless

  override def start(fiber: FiberImpl): Unit = drain(fiber)

  override def resume(fiber: FiberImpl): Unit = enqueue(fiber)

  @tailrec private def drain(fiber: FiberImpl): Unit =
    val yielder = fiber.run()
    if yielder.isPending then
      if isEmpty then
        drain(yielder)
      else
        enqueue(yielder)
        drain(dequeue())
    else
      if !isEmpty then
        drain(dequeue())

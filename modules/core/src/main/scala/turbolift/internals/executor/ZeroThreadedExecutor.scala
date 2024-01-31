package turbolift.internals.executor
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.internals.engine.{FiberImpl, FiberLink}


private[internals] final class ZeroThreadedExecutor extends FiberLink with Executor:
  {
    linkWithSelf()
  }

  override def start(fiber: FiberImpl): Unit =
    drain(fiber)
    fiber.doFinalize()

  override def enqueue(fiber: FiberImpl): Unit = insertLast(fiber)

  @tailrec private def drain(fiber: FiberImpl): Unit =
    val yielder = fiber.run()
    val isPending = yielder.isPending

    if isLinkedWithSelf then
      if isPending then
        drain(yielder)
      else
        ()
    else
      if isPending then
        insertLast(yielder)
      drain(removeFirst())

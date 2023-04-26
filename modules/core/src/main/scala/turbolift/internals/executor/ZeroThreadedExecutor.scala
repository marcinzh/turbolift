package turbolift.internals.executor
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.internals.engine.{Config, FiberImpl, FiberLink}


private[internals] final class ZeroThreadedExecutor extends FiberLink with Executor:
  {
    linkWithSelf()
  }

  override def start[A](comp: Computation[?, ?], config: Config): FiberImpl =
    val fiber = new FiberImpl(comp, config)
    drain(fiber)
    fiber

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


  // def kill(): Unit = linkWithSelf()
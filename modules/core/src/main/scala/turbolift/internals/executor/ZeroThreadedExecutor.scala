package turbolift.internals.executor
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.io.Outcome
import turbolift.internals.engine.{FiberImpl, WaiterLink, Halt}


private[internals] final class ZeroThreadedExecutor extends WaiterLink.Queue with Executor:
  private var isDone: Boolean = false

  override def toString = s"ZeroThreadedExecutor@${hashCode.toHexString}"


  override def runSync[A](comp: Computation[A, ?], name: String): Outcome[A] =
    var outcome: Outcome[A] = null.asInstanceOf[Outcome[A]]
    def callback(o: Outcome[A]): Unit =
      outcome = o
      isDone = true
    val fiber = FiberImpl.create(comp, this, name, isReentry = false, callback)
    drain(fiber)
    outcome


  override def runAsync[A](comp: Computation[A, ?], name: String, callback: Outcome[A] => Unit): Unit =
    callback(runSync(comp, name))


  private[turbolift] override def resume(fiber: FiberImpl): Unit =
    synchronized {
      val wasEmpty = isEmpty
      enqueue(fiber)
      if wasEmpty then
        notify()
    }


  @tailrec private def drain(fiber: FiberImpl): Unit =
    fiber.run() match
      case Halt.Yield(yielder) =>
        val next =
          synchronized {
            if isEmpty then
              yielder
            else
              enqueue(yielder)
              dequeue()
          }
        drain(next)

      case _ =>
        val next =
          synchronized {
            if isEmpty then
              if !isDone then
                wait()
                dequeue()
              else
                null
            else
              dequeue()
          }
        if next != null then
          drain(next)

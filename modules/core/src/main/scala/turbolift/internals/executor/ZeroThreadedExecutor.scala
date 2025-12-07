package turbolift.internals.executor
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.data.Outcome
import turbolift.internals.engine.{FiberImpl, WaiterLink}


private[internals] final class ZeroThreadedExecutor extends WaiterLink.Queue with Executor:
  private var isDone: Boolean = false
  private var isIdle: Boolean = false

  override def toString = s"ZeroThreadedExecutor@${hashCode.toHexString}"


  override def runSync[A](comp: Computation[A, ?], name: String): Outcome[A] =
    var outcome: Outcome[A] = null.asInstanceOf[Outcome[A]]
    def callback(o: Outcome[A]): Unit =
      outcome = o
      isDone = true
    val fiber = FiberImpl.createRoot(comp, this, name, isReentry = false, callback)
    drain(fiber)
    outcome


  override def runAsync[A](comp: Computation[A, ?], callback: Outcome[A] => Unit, name: String): Unit =
    callback(runSync(comp, name))


  private[turbolift] override def resume(fiber: FiberImpl): Unit =
    synchronized {
      enqueue(fiber)
      if isIdle then
        isIdle = false
        notify()
    }


  private def drain(initialFiber: FiberImpl): Unit =
    var currentFiber: FiberImpl | Null = initialFiber
    while currentFiber != null do
      currentFiber =
        currentFiber.runUntilYields() match
          case yielder: FiberImpl => 
            synchronized {
              if !isEmpty then
                enqueue(yielder)
                dequeue()
              else
                yielder
            }

          case _ => 
            synchronized {
              if isEmpty then
                if !isDone then
                  isIdle = true
                  wait()
                  dequeue()
                else
                  null
              else
                dequeue()
            }

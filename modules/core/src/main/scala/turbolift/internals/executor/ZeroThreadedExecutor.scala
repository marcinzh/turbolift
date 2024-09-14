package turbolift.internals.executor
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.io.Outcome
import turbolift.internals.engine.{Engine, Halt}
import turbolift.internals.engine.concurrent.{FiberImpl, WaiterLink}


private[internals] final class ZeroThreadedExecutor extends WaiterLink.Queue with Executor:
  private var isDone: Boolean = false
  private var isIdle: Boolean = false
  private val engine = new Engine { override def run() = () } //// `run` is not used by this executor

  override def toString = s"ZeroThreadedExecutor@${hashCode.toHexString}"


  override def runSync[A](comp: Computation[A, ?], name: String): Outcome[A] =
    var outcome: Outcome[A] = null.asInstanceOf[Outcome[A]]
    def callback(o: Outcome[A]): Unit =
      outcome = o
      isDone = true
    engine.become(FiberImpl.create(comp, this, name, isReentry = false, callback))
    drain()
    outcome


  override def runAsync[A](comp: Computation[A, ?], name: String, callback: Outcome[A] => Unit): Unit =
    callback(runSync(comp, name))


  private[turbolift] override def resume(fiber: FiberImpl): Unit =
    synchronized {
      enqueue(fiber)
      if isIdle then
        isIdle = false
        notify()
    }


  private def drain(): Unit =
    var keepGoing = true
    while keepGoing do
      engine.runCurrent() match
        case Halt.Yield =>
          val last = engine.getCurrentFiber
          val next =
            synchronized {
              if !isEmpty then
                enqueue(last)
                dequeue()
              else
                null
            }
          if next != null then
            engine.become(next)

        case _ =>
          val next =
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
          if next != null then
            engine.become(next)
          else
            engine.becomeClear()
            keepGoing = false

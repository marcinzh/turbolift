package turbolift.internals.executor
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.io.Outcome
import turbolift.internals.engine.{FiberImpl, WaiterLink, MainLoop, Halt}


private[internals] final class ZeroThreadedExecutor extends WaiterLink.Queue with Executor:
  private var isDone: Boolean = false
  private val mainLoop = new MainLoop { override def run() = () } //// `run` is not used by this executor

  override def toString = s"ZeroThreadedExecutor@${hashCode.toHexString}"


  override def runSync[A](comp: Computation[A, ?], name: String): Outcome[A] =
    var outcome: Outcome[A] = null.asInstanceOf[Outcome[A]]
    def callback(o: Outcome[A]): Unit =
      outcome = o
      isDone = true
    mainLoop.become(FiberImpl.create(comp, this, name, isReentry = false, callback))
    drain()
    outcome


  override def runAsync[A](comp: Computation[A, ?], name: String, callback: Outcome[A] => Unit): Unit =
    callback(runSync(comp, name))


  private[turbolift] override def resume(fiber: FiberImpl): Unit =
    synchronized {
      val wasEmpty = isEmpty
      enqueue(fiber)
      if wasEmpty then
        mainLoop.become(fiber)
        notify()
    }


  private def drain(): Unit =
    var keepGoing = true
    while keepGoing do
      mainLoop.runCurrent() match
        case Halt.Yield =>
          val last = mainLoop.getCurrentFiber
          val next =
            synchronized {
              if !isEmpty then
                enqueue(last)
                dequeue()
              else
                null
            }
          if next != null then
            mainLoop.become(next)

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
            mainLoop.become(next)
          else
            mainLoop.becomeClear()
            keepGoing = false

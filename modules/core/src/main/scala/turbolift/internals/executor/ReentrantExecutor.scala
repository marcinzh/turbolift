package turbolift.internals.executor
import java.lang.ThreadLocal
import java.util.concurrent.ArrayBlockingQueue
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.io.Outcome
import turbolift.internals.engine.{Engine, Halt}
import turbolift.internals.engine.concurrent.{FiberImpl, WaiterLink}


private[turbolift] final class ReentrantExecutor(maxBusyThreads: Int) extends WaiterLink.Queue with Executor:
  enclosing =>
  private var idleCounter: Int = maxBusyThreads
  protected val pad1, pad2, pad3 = 0L


  override def runSync[A](comp: Computation[A, ?], name: String): Outcome[A] =
    val queue = new ArrayBlockingQueue[Outcome[A]](1)
    runAsync(comp, name, queue.offer)
    queue.take().nn


  override def runAsync[A](comp: Computation[A, ?], name: String, callback: Outcome[A] => Unit): Unit =
    val isReentry = ReentrantExecutor.currentVar.get != null
    val fiber = FiberImpl.create(comp, this, name, isReentry, callback)
    if !isReentry then
      resume(fiber)
    else
      awaken(fiber)


  private[turbolift] override def resume(fiber: FiberImpl): Unit =
    val doAwaken =
      atomically {
        if idleCounter > 0 then
          idleCounter = idleCounter - 1
          true
        else
          enqueue(fiber)
          false
      }
    if doAwaken then
      awaken(fiber)


  private def awaken(initial: FiberImpl): Unit =
    Pool.instance.execute(new Run(initial))


  private final class Run(initial: FiberImpl) extends Engine(initial):
    override def run(): Unit =
      ReentrantExecutor.currentVar.set(enclosing)
      var keepGoing = true
      while keepGoing do
        runCurrent() match
          case Halt.Yield =>
            val last = getCurrentFiber
            val next =
              enclosing.atomically {
                if !isEmpty then
                  enqueue(last)
                  dequeue()
                else
                  null
              }
            if next != null then
              become(next)

          case _ =>
            val next =
              enclosing.atomically {
                if isEmpty then
                  if !getCurrentFiber.isReentry then
                    idleCounter = idleCounter + 1
                  null
                else
                  dequeue()
              }
            if next != null then
              become(next)
            else
              becomeClear()
              keepGoing = false


private[turbolift] object ReentrantExecutor:
  def apply(f: Int => Int): ReentrantExecutor =
    val cpus = Runtime.getRuntime.nn.availableProcessors()
    new ReentrantExecutor(f(cpus))

  lazy val default: ReentrantExecutor = apply(n => n)

  private val currentVar: ThreadLocal[ReentrantExecutor] = new ThreadLocal[ReentrantExecutor]

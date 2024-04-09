package turbolift.internals.executor
import java.lang.ThreadLocal
import java.util.concurrent.ArrayBlockingQueue
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.io.Outcome
import turbolift.internals.engine.{FiberImpl, WaiterLink, Halt}


private[turbolift] final class MultiThreadedExecutor(maxBusyThreads: Int) extends WaiterLink.Queue with Executor:
  enclosing =>
  private var idleCounter: Int = maxBusyThreads
  protected[this] val pad1, pad2, pad3, pad4 = 0L


  override def runSync[A](comp: Computation[A, ?], name: String): Outcome[A] =
    val queue = new ArrayBlockingQueue[Outcome[A]](1)
    runAsync(comp, name, queue.offer)
    queue.take().nn


  override def runAsync[A](comp: Computation[A, ?], name: String, callback: Outcome[A] => Unit): Unit =
    val isReentry = MultiThreadedExecutor.currentVar.get != null
    val fiber = FiberImpl.create(comp, this, name, isReentry, callback)
    if !isReentry then
      resume(fiber)
    else
      awaken(fiber)


  override def resume(fiber: FiberImpl): Unit =
    val doAwaken =
      synchronized {
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


  private final class Run(private var todo: FiberImpl | Null) extends Runnable:
    override def run(): Unit =
      MultiThreadedExecutor.currentVar.set(enclosing)
      while todo != null do
        val halt = todo.nn.run()
        todo = halt match
          case Halt.Yield(yielder) =>
            enclosing.synchronized {
              if isEmpty then
                yielder
              else
                enqueue(yielder)
                dequeue()
            }

          case Halt.Retire(reentry) =>
            enclosing.synchronized {
              if isEmpty then
                if !reentry then
                  idleCounter = idleCounter + 1
                null
              else
                dequeue()
            }


private[turbolift] object MultiThreadedExecutor:
  def apply(f: Int => Int): MultiThreadedExecutor =
    val cpus = Runtime.getRuntime.nn.availableProcessors()
    new MultiThreadedExecutor(f(cpus))

  val default: MultiThreadedExecutor = apply(n => n)

  private[this] val currentVar: ThreadLocal[MultiThreadedExecutor] = new ThreadLocal[MultiThreadedExecutor]

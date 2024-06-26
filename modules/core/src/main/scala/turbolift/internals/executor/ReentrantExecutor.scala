package turbolift.internals.executor
import java.lang.ThreadLocal
import java.util.concurrent.ArrayBlockingQueue
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.io.Outcome
import turbolift.internals.engine.{FiberImpl, WaiterLink, Halt}


private[turbolift] final class ReentrantExecutor(maxBusyThreads: Int) extends WaiterLink.Queue with Executor:
  enclosing =>
  private var idleCounter: Int = maxBusyThreads
  protected[this] val pad1, pad2, pad3 = 0L


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


  override def resume(fiber: FiberImpl): Unit =
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


  private final class Run(private var todo: FiberImpl | Null) extends Runnable:
    override def run(): Unit =
      ReentrantExecutor.currentVar.set(enclosing)
      while todo != null do
        val halt = todo.nn.run()
        todo = halt match
          case Halt.Yield(yielder) =>
            enclosing.atomically {
              if isEmpty then
                yielder
              else
                enqueue(yielder)
                dequeue()
            }

          case Halt.Retire(reentry) =>
            enclosing.atomically {
              if isEmpty then
                if !reentry then
                  idleCounter = idleCounter + 1
                null
              else
                dequeue()
            }


private[turbolift] object ReentrantExecutor:
  def apply(f: Int => Int): ReentrantExecutor =
    val cpus = Runtime.getRuntime.nn.availableProcessors()
    new ReentrantExecutor(f(cpus))

  lazy val default: ReentrantExecutor = apply(n => n)

  private[this] val currentVar: ThreadLocal[ReentrantExecutor] = new ThreadLocal[ReentrantExecutor]

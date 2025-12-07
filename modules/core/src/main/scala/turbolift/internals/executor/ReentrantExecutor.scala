package turbolift.internals.executor
import java.lang.ThreadLocal
import java.util.concurrent.ArrayBlockingQueue
import scala.annotation.tailrec
import turbolift.Computation
import turbolift.data.Outcome
import turbolift.internals.engine.{FiberImpl, WaiterLink}



private[turbolift] final class ReentrantExecutor(maxBusyThreads: Int) extends WaiterLink.Queue with Executor:
  enclosing =>
  private var idleCounter: Int = maxBusyThreads
  protected val pad1, pad2, pad3 = 0L


  override def runSync[A](comp: Computation[A, ?], name: String): Outcome[A] =
    val queue = new ArrayBlockingQueue[Outcome[A]](1)
    runAsync(comp, queue.offer, name)
    queue.take().nn


  override def runAsync[A](comp: Computation[A, ?], callback: Outcome[A] => Unit, name: String): Unit =
    val isReentry = ReentrantExecutor.currentVar.get != null
    val fiber = FiberImpl.createRoot(comp, this, name, isReentry, callback)
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


  @annotation.nowarn("msg=already not null") // for cross compiling LTS & Next
  //// LTS compiler doesn't infer non-null of `currentFiber`. Weird, same code in ZeroThreadExecutor compiles
  private final class Run(initialFiber: FiberImpl) extends Runnable:
    override def run(): Unit =
      var currentFiber: FiberImpl | Null = initialFiber
      ReentrantExecutor.currentVar.set(enclosing)
      while currentFiber != null do
        currentFiber =
          currentFiber.nn.runUntilYields() match
            case yielder: FiberImpl => 
              enclosing.atomically {
                if !isEmpty then
                  enqueue(yielder)
                  dequeue()
                else
                  null
                }

            case wasReentry: Boolean =>
              enclosing.atomically {
                if isEmpty then
                  if !wasReentry then
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

  private val currentVar: ThreadLocal[ReentrantExecutor] = new ThreadLocal[ReentrantExecutor]

package turbolift.internals.executor
import java.lang.ThreadLocal
import turbolift.Computation
import turbolift.internals.engine.{FiberImpl, Link}


private[turbolift] final class MultiThreadedExecutor(maxBusyThreads: Int) extends Link.Queue with Executor:
  private var idleCounter: Int = maxBusyThreads
  protected[this] val pad3, pad4, pad5, pad6, pad7 = 0L


  override def detectReentry(): Boolean =
    MultiThreadedExecutor.currentVar.get != null


  override def start(fiber: FiberImpl): Unit =
    if !fiber.isReentry then
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
      MultiThreadedExecutor.currentVar.set(MultiThreadedExecutor.this)
      while todo != null do
        val yielder = todo.nn.run()
        todo = take(yielder)


  private def take(yielder: FiberImpl): FiberImpl | Null =
    if yielder.isPending then
      synchronized {
        if isEmpty then
          yielder
        else
          enqueue(yielder)
          dequeue()
      }
    else
      synchronized {
        if isEmpty then
          if !yielder.isReentry then
            idleCounter = idleCounter + 1
          null
        else
          dequeue()
      }


object MultiThreadedExecutor:
  def apply(f: Int => Int): MultiThreadedExecutor =
    val cpus = Runtime.getRuntime.nn.availableProcessors()
    new MultiThreadedExecutor(f(cpus))

  val default: MultiThreadedExecutor = apply(n => n)

  private[this] val currentVar: ThreadLocal[MultiThreadedExecutor] = new ThreadLocal[MultiThreadedExecutor]

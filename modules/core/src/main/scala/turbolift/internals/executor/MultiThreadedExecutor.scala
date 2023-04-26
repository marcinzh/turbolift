package turbolift.internals.executor
import java.lang.ThreadLocal
import turbolift.Computation
import turbolift.internals.engine.{Config, FiberImpl, FiberLink}
import turbolift.internals.executor.Executor


private[turbolift] final class MultiThreadedExecutor(maxBusyThreads: Int) extends FiberLink with Executor:
  private var idleCounter: Int = maxBusyThreads
  protected[this] val pad3, pad4, pad5, pad6, pad7 = 0L

  {
    linkWithSelf()
  }


  override def start[A](comp: Computation[?, ?], config: Config): FiberImpl =
    val fiber = new FiberImpl(comp, config)
    val current = MultiThreadedExecutor.currentVar.get
    if current == null then
      enqueue(fiber)
    else
      fiber.setSubstitute()
      awaken(fiber)
    fiber.doWait()
    fiber


  override def enqueue(fiber: FiberImpl): Unit =
    val doAwaken =
      synchronized {
        if idleCounter > 0 then
          idleCounter = idleCounter - 1
          true
        else
          insertLast(fiber)
          false
      }
    if doAwaken then
      awaken(fiber)


  private def awaken(fiber: FiberImpl): Unit =
    Pool.instance.execute { () =>
      MultiThreadedExecutor.currentVar.set(MultiThreadedExecutor.this)
      var yielder: FiberImpl = null.asInstanceOf[FiberImpl]
      var todo: FiberImpl | Null = fiber
      while todo != null do
        yielder = todo.run()
        todo = dequeue(yielder)
      if yielder.isRoot then
        yielder.doNotify()
    }


  private def dequeue(yielder: FiberImpl): FiberImpl | Null =
    val isPending = yielder.isPending
    val isSubstitute = yielder.isSubstitute
    synchronized {
      if isLinkedWithSelf then
        if !isPending && !isSubstitute then
          idleCounter = idleCounter + 1
        if isPending then yielder else null
      else
        if isPending then
          insertLast(yielder)
        if !isSubstitute then removeFirst() else null
    }


object MultiThreadedExecutor:
  def apply(f: Int => Int): MultiThreadedExecutor =
    val cpus = Runtime.getRuntime.nn.availableProcessors()
    new MultiThreadedExecutor(f(cpus))

  val default: MultiThreadedExecutor = apply(n => n)

  private[this] val currentVar: ThreadLocal[MultiThreadedExecutor] = new ThreadLocal[MultiThreadedExecutor]

package turbolift.internals.engine.concurrent
import java.util.concurrent.{TimeUnit, Future}
import java.util.concurrent.atomic.AtomicReference
import turbolift.io.Exceptions
import turbolift.internals.executor.{Pool, Scheduler}


private[engine] sealed trait Blocker:
  def unblock(): Unit

  def isEither: Boolean = false
  def getCompletion: Byte = Bits.Completion_Success
  def getResult: Any = ()
  final def getThrowable: Throwable = getResult.asInstanceOf[Throwable]


private[engine] object Blocker:
  trait Unsealed extends Blocker
  /*private*/ case object Done
  /*private*/ type Done = Done.type


  final class Zombie(fiber: FiberImpl) extends Blocker:
    override def unblock(): Unit = fiber.resume()


  final class Sleeper(fiber: FiberImpl) extends AtomicReference[Future[?] | Done | Null] with Blocker with Runnable:
    def sleep(length: Long, unit: TimeUnit): Unit =
      val future = Scheduler.instance.schedule(this, length, unit).nn
      if compareAndSet(null, future) then
        ()
      else
        future.cancel(true)


    override def run: Unit =
      getAndSet(Done) match
        case Done => ()
        case _ => fiber.resume()


    override def unblock(): Unit =
      compareAndExchange(null, Done) match
        case null => fiber.resume()
        case future: Future[?] => future.cancel(true); fiber.resume()
        case Done => ()



  final class Interruptible(fiber: FiberImpl, thunk: () => Any, override val isEither: Boolean) extends AtomicReference[Thread | Done | Null] with Blocker with Runnable:
    private var result: Any = null
    private var completion: Byte = Bits.Completion_Success
    override def getCompletion: Byte = completion
    override def getResult: Any = result


    def block(): Unit = Pool.instance.execute(this)


    override def run: Unit =
      val thread = Thread.currentThread.nn
      if compareAndSet(null, thread) then
        try
          result = thunk()
        catch 
          case _: InterruptedException =>
            completion = Bits.Completion_Cancelled
          case e =>
            completion = Bits.Completion_Failure
            result = e
        finally
          set(Done)
      fiber.resume()


    override def unblock(): Unit =
      compareAndExchange(null, Done) match
        case thread: Thread => thread.interrupt()
        case _ => ()

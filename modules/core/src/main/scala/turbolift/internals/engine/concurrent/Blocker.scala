package turbolift.internals.engine.concurrent
import java.util.concurrent.{TimeUnit, Future}
import java.util.concurrent.atomic.AtomicReference
import turbolift.internals.executor.{Pool, Scheduler}


private[engine] sealed trait Blocker:
  var result: Any = ()
  var throwable: Throwable | Null = null
  def unblock(): Unit


  final def toEither: Either[Throwable, Any] =
    val e = throwable
    if e == null then
      Right(result)
    else
      Left(e)


private[engine] object Blocker:
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



  final class Interruptible(fiber: FiberImpl, thunk: () => Any) extends AtomicReference[Thread | Done | Null] with Blocker with Runnable:
    def block(): Unit = Pool.instance.execute(this)


    override def run: Unit =
      val thread = Thread.currentThread.nn
      if compareAndSet(null, thread) then
        try
          this.result = thunk()
          set(Done)
        catch 
          case _: InterruptedException => ()
          case e => this.throwable = e
      end if
      fiber.resume()


    override def unblock(): Unit =
      compareAndExchange(null, Done) match
        case thread: Thread => thread.interrupt()
        case _ => ()

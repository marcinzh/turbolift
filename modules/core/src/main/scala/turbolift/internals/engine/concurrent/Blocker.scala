package turbolift.internals.engine.concurrent
import java.util.concurrent.{TimeUnit, Future}
import java.util.concurrent.atomic.AtomicReference
import turbolift.data.Exceptions
import turbolift.internals.executor.{Pool, Scheduler}


private[engine] sealed trait Blocker:
  def unblock(): Unit


private[engine] object Blocker:
  trait Unsealed extends Blocker
  /*private*/ case object Done
  /*private*/ type Done = Done.type


  final class Zombie(fiber: FiberImpl) extends Blocker:
    override def unblock(): Unit = fiber.resumeWaiter()


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
        case _ => fiber.resumeWaiter()


    override def unblock(): Unit =
      compareAndExchange(null, Done) match
        case null => fiber.resume()
        case future: Future[?] => future.cancel(true); fiber.resumeWaiter()
        case Done => ()


  final class Interruptible(fiber: FiberImpl, thunk: () => Any, isEither: Boolean) extends AtomicReference[Thread | Done | Null] with Blocker with Runnable:
    def block(): Unit = Pool.instance.execute(this)

    override def run: Unit =
      val thread = Thread.currentThread.nn
      if compareAndSet(null, thread) then
        var throwable: Throwable | Null = null
        var value: Any = null
        try
          value = thunk()
        catch
          case e => throwable = e
        finally
          set(Done)

        if isEither then
          val value2 = if throwable == null then Right(value) else Left(throwable.nn)
          fiber.resumeWaiterAsSuccess(value2)
        else
          if throwable == null then
            fiber.resumeWaiterAsSuccess(value)
          else
            fiber.resumeWaiterAsFailure(throwable.nn)
      else
        fiber.resumeWaiter()


    override def unblock(): Unit =
      compareAndExchange(null, Done) match
        case thread: Thread => thread.interrupt()
        case _ => ()

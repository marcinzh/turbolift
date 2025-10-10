package turbolift.internals.engine.concurrent
import scala.annotation.tailrec
import turbolift.io.Semaphore
import turbolift.internals.engine.{Waitee, FiberImpl, Halt}


private[turbolift] final class SemaphoreImpl(private var permits: Long) extends Waitee with Semaphore.Unsealed:
  override def intrinsicAcquire(waiter: FiberImpl, count: Long): Halt =
    waiter.willContinuePure(())
    waiter.theWaiterStateLong = count 

    atomicallyBoth(waiter) {
      if firstWaiter == null then
        val n = permits - count
        if n >= 0 then
          permits = n
          Halt.Continue
        else
          subscribeFirstWaiterUnsync(waiter)
          Halt.Retire
      else
        subscribeWaiterUnsync(waiter)
        Halt.Retire
    }


  override def unsafeTryAcquire(count: Long): Boolean =
    atomically {
      if firstWaiter == null then
        val n = permits - count
        if n >= 0 then
          permits = n
          true
        else
          false
      else
        false
    }


  @tailrec override def unsafeRelease(count: Long): Unit =
    var waiterToResume: FiberImpl | Null = null

    val keepGoing =
      atomically {
        permits += count
        val x = firstWaiter
        if x != null then
          val n = permits - x.theWaiterStateLong
          if n >= 0 then
            permits = n
            waiterToResume = x
            removeFirstWaiter()
            x.standbyWaiter()
          n > 0
        else
          false
      }

    if waiterToResume != null then
      waiterToResume.nn.resume()

    if keepGoing then
      unsafeRelease(0)


  protected override def afterUnsubscribe(): Unit = unsafeRelease(0)

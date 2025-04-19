package turbolift.internals.engine.concurrent.util
import scala.annotation.tailrec
import turbolift.io.Semaphore
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}


private[turbolift] final class SemaphoreImpl(private var permits: Long) extends Waitee with Semaphore.Unsealed:
  def tryGetAcquiredBy(waiter: FiberImpl, isWaiterCancellable: Boolean, count: Long): Int =
    atomicallyBoth(waiter, isWaiterCancellable) {
      if firstWaiter == null then
        val n = permits - count
        if n >= 0 then
          permits = n
          Bits.WaiteeAlreadyCompleted
        else
          subscribeFirstWaiterUnsync(waiter)
          Bits.WaiterSubscribed
      else
        subscribeWaiterUnsync(waiter)
        Bits.WaiterSubscribed
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
    var savedWaiter: FiberImpl | Null = null

    val keepGoing =
      atomically {
        permits += count
        val x = firstWaiter
        if x != null then
          val n = permits - x.payloadAs[Long]
          if n >= 0 then
            permits = n
            savedWaiter = x
            removeFirstWaiter()
            x.standbyWaiterPure(())
          n > 0
        else
          false
      }

    if savedWaiter != null then
      savedWaiter.nn.resume()

    if keepGoing then
      unsafeRelease(0)


  protected override def afterUnsubscribe(): Unit = unsafeRelease(0)

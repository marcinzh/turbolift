package turbolift.internals.engine.concurrent.util
import turbolift.io.Semaphore
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}


private[turbolift] final class SemaphoreImpl(private var permits: Long) extends Waitee with Semaphore.Unsealed:

  def tryGetAcquiredBy(waiter: FiberImpl, isWaiterCancellable: Boolean, count: Long): Int =
    atomicallyBoth(waiter, isWaiterCancellable) {
      val n: Long = permits - count
      if n < 0 then
        permits = 0
        waiter.suspendedPayload = -n
        subscribeWaiterUnsync(waiter)
        Bits.WaiterSubscribed
      else
        permits = n
        Bits.WaiteeAlreadyCompleted
    }


  override def unsafeRelease(count: Long): Unit =
    var savedFirstWaiter: FiberImpl | Null = null
    var savedLastWaiter: FiberImpl | Null = null

    atomically {
      if firstWaiter == null then
        permits += count
      else
        @annotation.tailrec def loop(curr: FiberImpl, prev: FiberImpl | Null, remaining: Long): Unit =
          val n = remaining - curr.suspendedPayload.asInstanceOf[Long]
          if n >= 0 then
            savedLastWaiter = curr
            val next = curr.nextWaiter.nn.asFiber
            if next == firstWaiter then
              //// all waiters are released
              permits = n
              savedFirstWaiter = firstWaiter
              firstWaiter = null
            else
              loop(next, curr, n)
          else
            //// some or all waiters remain
            permits = 0
            curr.suspendedPayload = -n
            if prev == null then
              //// all waiters remain
              ()
            else
              //// some waiters remain and some are released
              savedFirstWaiter = firstWaiter
              savedLastWaiter = prev
              firstWaiter.nn.removeRangeOfWaitersAtSelf(prev)
              firstWaiter = curr

        loop(firstWaiter.nn, null, count)
    }

    if savedFirstWaiter != null then
      if savedLastWaiter != null then
        Waitee.notifyRangeOfWaiters(savedFirstWaiter.nn, savedLastWaiter.nn)
      else
        Waitee.notifyAllWaiters(savedFirstWaiter.nn)

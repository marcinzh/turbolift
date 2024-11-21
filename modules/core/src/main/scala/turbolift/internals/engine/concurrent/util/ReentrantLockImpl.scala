package turbolift.internals.engine.concurrent.util
import turbolift.io.ReentrantLock
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}


private[turbolift] final class ReentrantLockImpl extends Waitee with ReentrantLock.Unsealed:
  private var lockedBy: FiberImpl | Null = null
  private var reentryCount: Int = 0


  def tryGetAcquiredBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    atomicallyBoth(waiter, isWaiterCancellable) {
      if reentryCount == 0 then
        lockedBy = waiter
        reentryCount = 1
        Bits.WaiteeAlreadyCompleted
      else
        if lockedBy == waiter then
          reentryCount += 1
          Bits.WaiteeAlreadyCompleted
        else
          subscribeWaiterUnsync(waiter)
          Bits.WaiterSubscribed
    }


  override def unsafeRelease(): Unit =
    var savedWaiter: FiberImpl | Null = null

    atomically {
      reentryCount -= 1
      if reentryCount == 0 then
        val x = firstWaiter
        if x == null then
          lockedBy = null
        else
          reentryCount = 1
          lockedBy = x
          savedWaiter = x
          removeFirstWaiter()
          x.standbyWaiterPure(())
    }

    if savedWaiter != null then
      savedWaiter.nn.resume()


  def unsafeStatus(): ReentrantLock.Status =
    var savedLockedBy: FiberImpl | Null = null
    var savedReentryCount: Int = 0

    atomically {
      savedLockedBy = lockedBy
      savedReentryCount = reentryCount
    }

    if savedLockedBy == null then
      ReentrantLock.Status.Unlocked
    else
      ReentrantLock.Status.Locked(owner = savedLockedBy.nn, holdCount = savedReentryCount)

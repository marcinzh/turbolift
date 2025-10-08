package turbolift.internals.engine.concurrent
import turbolift.io.{ReentrantLock, Fiber}
import turbolift.internals.engine.{Waitee, FiberImpl, Halt}
import turbolift.internals.engine.Misc.asImpl


private[turbolift] final class ReentrantLockImpl extends Waitee with ReentrantLock.Unsealed:
  private var lockedBy: FiberImpl | Null = null
  private var reentryCount: Int = 0


  override def intrinsicAcquire(waiter: FiberImpl): Halt =
    waiter.willContinuePure(())

    atomicallyBoth(waiter) {
      if reentryCount == 0 then
        lockedBy = waiter
        reentryCount = 1
        Halt.Continue
      else
        if lockedBy == waiter then
          reentryCount += 1
          Halt.Continue
        else
          subscribeWaiterUnsync(waiter)
          Halt.Retire
    }


  override def unsafeTryAcquire(owner: Fiber.Untyped): Boolean =
    val waiter = owner.asImpl

    atomically {
      if reentryCount == 0 then
        lockedBy = waiter
        reentryCount = 1
        true
      else
        if lockedBy == waiter then
          reentryCount += 1
          true
        else
          false
    }


  override def unsafeRelease(): Unit =
    var waiterToResume: FiberImpl | Null = null

    atomically {
      if reentryCount > 0 then
        reentryCount -= 1
        if reentryCount == 0 then
          val x = firstWaiter
          if x == null then
            lockedBy = null
          else
            reentryCount = 1
            lockedBy = x
            waiterToResume = x
            removeFirstWaiter()
            x.standbyWaiter()
    }

    if waiterToResume != null then
      waiterToResume.nn.resume()


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

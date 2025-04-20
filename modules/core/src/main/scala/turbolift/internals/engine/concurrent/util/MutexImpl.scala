package turbolift.internals.engine.concurrent.util
import turbolift.io.Mutex
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}


private[turbolift] final class MutexImpl extends Waitee with Mutex.Unsealed:
  private var locked: Boolean = false


  def tryGetAcquiredBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    atomicallyBoth(waiter, isWaiterCancellable) {
      if locked then
        subscribeWaiterUnsync(waiter)
        Bits.WaiterSubscribed
      else
        locked = true
        Bits.WaiteeAlreadyCompleted
    }


  def unsafeTryAcquire(): Boolean =
    atomically {
      if !locked then
        locked = true
        true
      else
        false
    }


  def unsafeIsLocked(): Boolean =
    atomically {
      locked
    }


  override def unsafeRelease(): Unit =
    var savedWaiter: FiberImpl | Null = null

    atomically {
      val x = firstWaiter
      if x == null then
        locked = false
      else
        savedWaiter = x
        removeFirstWaiter()
        x.standbyWaiterPure(())
    }

    if savedWaiter != null then
      savedWaiter.nn.resume()

package turbolift.internals.engine.concurrent.util
import turbolift.io.Mutex
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}


private[turbolift] final class MutexImpl extends Waitee with Mutex.Unsealed:
  private var isLocked: Boolean = false


  def tryGetAcquiredBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    atomicallyBoth(waiter, isWaiterCancellable) {
      if isLocked then
        subscribeWaiterUnsync(waiter)
        Bits.WaiterSubscribed
      else
        isLocked = true
        Bits.WaiteeAlreadyCompleted
    }


  override def unsafeRelease(): Unit =
    var savedFirstWaiter: FiberImpl | Null = null

    atomically {
      if firstWaiter == null then
        isLocked = false
      else
        savedFirstWaiter = firstWaiter
    }

    if savedFirstWaiter != null then
      savedFirstWaiter.nn.resume()

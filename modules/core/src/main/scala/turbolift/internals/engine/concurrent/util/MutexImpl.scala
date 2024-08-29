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
    var savedWaiter: FiberImpl | Null = null

    atomically {
      val x = firstWaiter
      if x == null then
        isLocked = false
      else
        savedWaiter = x
        removeFirstWaiter()
        x.standbyWaiter(())
    }

    if savedWaiter != null then
      savedWaiter.nn.resume()

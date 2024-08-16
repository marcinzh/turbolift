package turbolift.internals.engine.concurrent.util
import turbolift.io.CountDownLatch
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}


private[turbolift] final class CountDownLatchImpl(private var counter: Int) extends Waitee with CountDownLatch.Unsealed:
  def tryGetAwaitedBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    atomicallyBoth(waiter, isWaiterCancellable) {
      if counter > 0 then
        subscribeWaiterUnsync(waiter)
        Bits.WaiterSubscribed
      else
        Bits.WaiteeAlreadyCompleted
    }


  override def unsafeRelease(): Unit =
    val willFinalize =
      atomically {
        val n = counter - 1
        if n >= 0 then
          counter = n
          n == 0
        else
          false
      }

    if willFinalize then
      notifyAllWaiters()

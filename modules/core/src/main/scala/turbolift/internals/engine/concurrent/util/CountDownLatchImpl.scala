package turbolift.internals.engine.concurrent.util
import turbolift.io.CountDownLatch
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}


private[turbolift] final class CountDownLatchImpl(private var counter: Int) extends Waitee with CountDownLatch.Unsealed:
  {
    if counter <= 0 then
      setCompletionToSuccess()
  }

  def tryGetAwaitedBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    atomicallyBoth(waiter, isWaiterCancellable) {
      if isPending then
        subscribeWaiterUnsync(waiter)
        Bits.WaiterSubscribed
      else
        Bits.WaiteeAlreadyCompleted
    }


  override def unsafeRelease(): Unit =
    val willFinalize =
      atomically {
        if isPending then
          counter -= 1
          if counter == 0 then
            setCompletionToSuccess()
            true
          else
            false
        else
          false
      }

    if willFinalize then
      finallyNotifyAllWaiters()

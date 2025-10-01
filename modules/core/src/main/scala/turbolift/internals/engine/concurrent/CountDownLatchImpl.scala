package turbolift.internals.engine.concurrent
import turbolift.io.CountDownLatch
import turbolift.internals.engine.{Waitee, FiberImpl, Halt}


private[turbolift] final class CountDownLatchImpl(private var counter: Int) extends Waitee with CountDownLatch.Unsealed:
  {
    if counter <= 0 then
      setCompletionToSuccess()
  }

  def tryGetAwaitedBy(waiter: FiberImpl): Halt =
    atomicallyBoth(waiter) {
      if isPending then
        subscribeWaiterUnsync(waiter)
        Halt.Retire
      else
        Halt.Continue
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

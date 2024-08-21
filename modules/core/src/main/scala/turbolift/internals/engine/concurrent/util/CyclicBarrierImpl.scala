package turbolift.internals.engine.concurrent.util
import turbolift.io.CyclicBarrier
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}


private[turbolift] final class CyclicBarrierImpl(private val capacity: Int) extends Waitee with CyclicBarrier.Unsealed:
  private var counter = capacity


  def tryGetAwaitedBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    var savedFirstWaiter: FiberImpl | Null = null

    val result =
      atomicallyBoth(waiter, isWaiterCancellable) {
        val n = counter - 1
        if n > 0 then
          counter = n
          subscribeWaiterUnsync(waiter)
          Bits.WaiterSubscribed
        else
          counter = capacity
          savedFirstWaiter = firstWaiter
          firstWaiter = null
          Bits.WaiteeAlreadyCompleted
      }

    if savedFirstWaiter != null then
      Waitee.notifyAllWaiters(savedFirstWaiter.nn)
    result

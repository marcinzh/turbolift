package turbolift.internals.engine.concurrent.util
import scala.annotation.tailrec
import turbolift.io.CyclicBarrier
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}


private[turbolift] final class CyclicBarrierImpl(private val capacity: Int) extends Waitee with CyclicBarrier.Unsealed:
  private var counter = capacity
  private var clock = 0


  def tryGetAwaitedBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    var staleClock = 0

    val result =
      atomicallyBoth(waiter, isWaiterCancellable) {
        val n = counter - 1
        if n > 0 then
          counter = n
          waiter.payloadAs_=[Int](clock)
          subscribeWaiterUnsync(waiter)
          Bits.WaiterSubscribed
        else
          counter = capacity
          staleClock = clock
          clock += 1 //// wrap around
          Bits.WaiteeAlreadyCompleted
      }

    if result == Bits.WaiteeAlreadyCompleted then
      release(staleClock)
    result


  @tailrec private def release(staleClock: Int): Unit =
    var savedWaiter: FiberImpl | Null = null
  
    val keepGoing =
      atomically {
        val x = firstWaiter
        if x != null && x.payloadAs[Int] == staleClock then
          savedWaiter = x
          removeFirstWaiter()
          x.standbyWaiterPure(())
          true
        else
          false
      }

    if savedWaiter != null then
      savedWaiter.nn.resume()

    if keepGoing then
      release(staleClock)

package turbolift.internals.engine.concurrent
import scala.annotation.tailrec
import turbolift.io.CyclicBarrier
import turbolift.internals.engine.{Waitee, FiberImpl, Halt}


private[turbolift] final class CyclicBarrierImpl(private val capacity: Int) extends Waitee with CyclicBarrier.Unsealed:
  private var counter = capacity
  private var clock = 0L


  def tryGetAwaitedBy(waiter: FiberImpl): Halt =
    var staleClock = 0L

    val result =
      atomicallyBoth(waiter) {
        val n = counter - 1
        if n > 0 then
          counter = n
          waiter.theWaiterStateLong = clock
          subscribeWaiterUnsync(waiter)
          Halt.Retire
        else
          counter = capacity
          staleClock = clock
          clock += 1 //// wrap around
          Halt.Continue
      }

    if result == Halt.Continue then
      release(staleClock)
    result


  @tailrec private def release(staleClock: Long): Unit =
    var savedWaiter: FiberImpl | Null = null
  
    val keepGoing =
      atomically {
        val x = firstWaiter
        if x != null && x.theWaiterStateLong == staleClock then
          savedWaiter = x
          removeFirstWaiter()
          x.standbyWaiter()
          true
        else
          false
      }

    if savedWaiter != null then
      savedWaiter.nn.resume()

    if keepGoing then
      release(staleClock)

package turbolift.internals.engine.concurrent
import scala.annotation.tailrec
import turbolift.io.CyclicBarrier
import turbolift.internals.engine.{Waitee, FiberImpl, Halt}


private[turbolift] final class CyclicBarrierImpl(private val capacity: Int) extends Waitee with CyclicBarrier.Unsealed:
  private var counter = capacity
  private var clock = 0L


  override def intrinsicAwait(waiter: FiberImpl): Halt =
    waiter.willContinuePure(())
    var staleClock = 0L

    val halt =
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

    if halt == Halt.Continue then
      release(staleClock)
    halt


  @tailrec private def release(staleClock: Long): Unit =
    var waiterToResume: FiberImpl | Null = null
  
    val keepGoing =
      atomically {
        val x = firstWaiter
        if x != null && x.theWaiterStateLong == staleClock then
          waiterToResume = x
          removeFirstWaiter()
          x.standbyWaiter()
          true
        else
          false
      }

    if waiterToResume != null then
      waiterToResume.nn.resume()

    if keepGoing then
      release(staleClock)

package turbolift.internals.engine.concurrent
import scala.annotation.tailrec
import turbolift.io.CyclicBarrier
import turbolift.internals.engine.{Waitee, FiberImpl, Halt}


private[turbolift] final class CyclicBarrierImpl(private val capacity: Int) extends Waitee with CyclicBarrier.Unsealed:
  private var counter = capacity
  private var clock = 0


  override def intrinsicAwait(waiter: FiberImpl): Halt =
    waiter.willContinuePure(())
    var staleClock = 0

    val halt =
      atomicallyBoth(waiter) {
        val n = counter - 1
        if n > 0 then
          counter = n
          waiter.setWaiterStateInt(clock)
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


  @tailrec private def release(staleClock: Int): Unit =
    var waiterToResume: FiberImpl | Null = null
  
    val keepGoing =
      atomically {
        val x = theFirstWaiter
        if x != null && x.getWaiterStateInt == staleClock then
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

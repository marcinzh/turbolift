package turbolift.internals.engine
import scala.annotation.tailrec


/** Either Fiber, Warp, Queque or OnceVar */

private abstract class Waitee:
  protected var firstWaiter: FiberImpl | Null = null
  protected var varyingBits: Byte = 0
  protected var theOwnership: Byte = 0 //// meaningful only in FiberImpl, but moved here for convenience


  final def isPending: Boolean = Bits.isPending(varyingBits)
  final def isCancelled: Boolean = Bits.isCancellationSignalled(varyingBits)
  final def isPendingAndNotCancelled: Boolean = Bits.isPendingAndNotCancelled(varyingBits)


  //// Called only from synchonized blocks 
  final def subscribeWaiterUnsync(waiter: FiberImpl): Unit =
    val x = firstWaiter
    if x == null then
      firstWaiter = waiter
      waiter.linkWaiterWithSelf()
    else
      x.insertWaiterBeforeSelf(waiter)
    waiter.theOwnership = Bits.Ownership_Waitee


  final def unsubscribeWaiter(waiter: FiberImpl): Unit =
    val willResume =
      synchronized {
        if isPending then
          if waiter.isWaiterLinkedWithSelf then
            firstWaiter = null
          else
            if waiter == firstWaiter then
              firstWaiter = waiter.nextWaiter.nn.asFiber
            waiter.removeWaiterAtSelf()
          //// Not necessary but makes `status` more accurate
          waiter.theOwnership = Bits.Ownership_Self
          waiter.clearWaiterLink()
          true
        else
          false
      }

    if willResume then
      waiter.resume()


  final def notifyAllWaiters(): Unit =
    val x = firstWaiter
    if x != null then
      firstWaiter = null
      //@#@OPTY use `executor.resumeMany`, but only when all waiters are on the same executor
      @tailrec def loop(waiter: FiberImpl): Unit =
        waiter.resume()
        val next = waiter.nextWaiter.nn.asFiber
        if next != x then
          loop(next)
      loop(x)

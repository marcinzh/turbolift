package turbolift.internals.engine
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec


/** Either Fiber, Warp, Queque or OnceVar */

private abstract class Waitee extends AtomicBoolean:
  protected var firstWaiter: FiberImpl | Null = null
  protected var varyingBits: Byte = 0
  /*protected*/ var theOwnership: Byte = 0 //// meaningful only in FiberImpl, but moved here for convenience


  final def isPending: Boolean = Bits.isPending(varyingBits)
  final def isCancelled: Boolean = Bits.isCancellationSignalled(varyingBits)
  final def isPendingAndNotCancelled: Boolean = Bits.isPendingAndNotCancelled(varyingBits)


  //-------------------------------------------------------------------
  // atomically
  //-------------------------------------------------------------------


  inline final protected def spinAcquire(): Boolean = compareAndSet(false, true)
  inline final protected def spinRelease(): Unit = set(false)


  inline final def atomically[A](inline body: => A): A =
    while !spinAcquire() do ()
    val a = body
    spinRelease()
    a


  inline final def atomicallyIfNotCancelled(fiber: FiberImpl)(inline body: => Int): Int =
    if spinAcquireBoth(fiber) then
      val a = body
      spinReleaseBoth(fiber)
      a
    else
      Bits.WaiterAlreadyCancelled


  final private def spinAcquireBoth(fiber: FiberImpl): Boolean =
    @tailrec def loop(): Boolean =
      if fiber.spinAcquire() then
        if fiber.isCancellationUnlatched then
          fiber.setCancellationLatch()
          fiber.spinRelease()
          false
        else
          if spinAcquire() then
            true
          else
            fiber.spinRelease()
            loop()
      else
        loop()
    loop()


  final private def spinReleaseBoth(fiber: FiberImpl): Unit =
    fiber.spinRelease()
    spinRelease()


  //-------------------------------------------------------------------
  // Waiter & Waitee
  //-------------------------------------------------------------------


  //// Called only from `atomically` blocks
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
      atomically {
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

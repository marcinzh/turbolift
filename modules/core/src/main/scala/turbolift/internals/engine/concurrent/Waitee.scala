package turbolift.internals.engine.concurrent
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec


/** Either Fiber, Warp, Queque or OnceVar */

private abstract class Waitee extends AtomicBoolean:
  protected var firstWaiter: FiberImpl | Null = null
  protected var varyingBits: Byte = 0
  /*protected*/ var theOwnership: Byte = 0 //// meaningful only in FiberImpl, but moved here for convenience


  //-------------------------------------------------------------------
  // bits
  //-------------------------------------------------------------------


  private[engine] final def isPending: Boolean = Bits.isPending(varyingBits)
  private[engine] final def isCancelled: Boolean = Bits.isCancellationSignalled(varyingBits)
  private[engine] final def isPendingAndNotCancelled: Boolean = Bits.isPendingAndNotCancelled(varyingBits)
  private[engine] final def isCancellationUnlatched: Boolean = Bits.isCancellationUnlatched(varyingBits)


  inline protected final def setCancellationLatch(): Unit =
    varyingBits = (varyingBits | Bits.Cancellation_Latch_Bug).toByte


  //-------------------------------------------------------------------
  // atomically
  //-------------------------------------------------------------------


  inline protected final def spinAcquire(): Boolean = compareAndSet(false, true)
  inline protected final def spinRelease(): Unit = set(false)


  inline final def atomically[A](inline body: => A): A =
    while !spinAcquire() do ()
    val a = body
    spinRelease()
    a


  inline final def atomicallyTry[A](isCancellable: Boolean)(inline body: => Unit): Boolean =
    atomically {
      if isCancellable && isCancellationUnlatched then
        setCancellationLatch()
        false
      else
        body
        true
    }


  inline final def atomicallyBoth(fiber: FiberImpl, isFiberCancellable: Boolean)(inline body: => Int): Int =
    if spinAcquireBoth(fiber, isFiberCancellable) then
      val a = body
      spinReleaseBoth(fiber)
      a
    else
      Bits.WaiterAlreadyCancelled


  final private def spinAcquireBoth(fiber: FiberImpl, isFiberCancellable: Boolean): Boolean =
    @tailrec def loop(): Boolean =
      if fiber.spinAcquire() then
        if isFiberCancellable && fiber.isCancellationUnlatched then
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
    waiter.theWaitee = this


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
      Waitee.notifyAllWaitersLoop(x)


  final def removeFirstWaiter(): Unit =
    val x = firstWaiter.nn
    val y = x.nextWaiter
    if x == y then
      firstWaiter = null
    else
      firstWaiter = y.nn.asFiber
    x.removeWaiterAtSelf()


private[concurrent] object Waitee:
  def notifyAllWaitersLoop(firstWaiter: FiberImpl): Unit =
    //@#@OPTY use `executor.resumeMany`, but only when all waiters are on the same executor
    @tailrec def loop(waiter: FiberImpl): Unit =
      waiter.resume()
      val next = waiter.nextWaiter.nn.asFiber
      if next != firstWaiter then
        loop(next)
    loop(firstWaiter)

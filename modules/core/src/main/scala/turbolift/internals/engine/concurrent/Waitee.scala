package turbolift.internals.engine.concurrent
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec


/** Either Fiber, Warp, Queque or OnceVar */

private[engine] abstract class Waitee extends SpinLock:
  protected var firstWaiter: FiberImpl | Null = null
  protected var varyingBits: Byte = 0


  //-------------------------------------------------------------------
  // bits
  //-------------------------------------------------------------------


  private[engine] final def isPending: Boolean = Bits.isPending(varyingBits)
  private[engine] final def isCancelled: Boolean = Bits.isCancellationSignalled(varyingBits)
  private[engine] final def isPendingAndNotCancelled: Boolean = Bits.isPendingAndNotCancelled(varyingBits)
  private[engine] final def isCancellationUnlatched: Boolean = Bits.isCancellationUnlatched(varyingBits)


  //// {{ meaningful only in FiberImpl, but moved here for convenience

  inline protected final def setCancellationLatch(): Unit =
    varyingBits = (varyingBits | Bits.Cancellation_Latch_Bug).toByte

  inline protected final def clearOwnership(): Unit =
    varyingBits = (varyingBits & ~Bits.Ownership_Mask_Bug).toByte

  inline protected final def setOwnershipToBlocker(): Unit =
    varyingBits = (varyingBits | Bits.Ownership_Blocker_Bug).toByte

  inline protected final def setOwnershipToWaitee(): Unit =
    varyingBits = (varyingBits | Bits.Ownership_Waitee_Bug).toByte

  //// }}


  //-------------------------------------------------------------------
  // atomically
  //-------------------------------------------------------------------


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
    waiter.setOwnershipToWaitee()
    waiter.theWaitee = this


  //// Simpler `subscribeWaiterUnsync`, for when the caller knows that waiter list is empty
  final def subscribeFirstWaiterUnsync(waiter: FiberImpl): Unit =
    firstWaiter = waiter
    waiter.linkWaiterWithSelf()
    waiter.setOwnershipToWaitee()
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
          waiter.clearOwnership()
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
      Waitee.notifyAllWaiters(x)


  final def removeFirstWaiter(): Unit =
    val x = firstWaiter.nn
    val y = x.nextWaiter
    if x == y then
      firstWaiter = null
    else
      firstWaiter = y.nn.asFiber
    x.removeWaiterAtSelf()


private[concurrent] object Waitee:
  def notifyAllWaiters(first: FiberImpl): Unit = notifyRangeOfWaiters(first, first.prevWaiter.nn.asFiber)


  def notifyRangeOfWaiters(first: FiberImpl, last: FiberImpl): Unit =
    //@#@OPTY use `executor.resumeMany`, but only when all waiters are on the same executor
    @tailrec def loop(waiter: FiberImpl): Unit =
      waiter.resume()
      if waiter != last then
        loop(waiter.nextWaiter.nn.asFiber)
    loop(first)
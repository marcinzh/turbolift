package turbolift.internals.engine.concurrent
import turbolift.internals.engine.concurrent.atomic.AtomicBoolVH
import scala.annotation.tailrec


/** Either Fiber, Warp, Queque or OnceVar */

private[engine] abstract class Waitee extends AtomicBoolVH(false):
  protected var firstWaiter: FiberImpl | Null = null
  protected var varyingBits: Byte = 0


  //-------------------------------------------------------------------
  // bits
  //-------------------------------------------------------------------


  private[engine] final def isPending: Boolean = Bits.isPending(varyingBits)
  private[engine] final def isCancelled: Boolean = Bits.isCancellationSignalled(varyingBits)
  private[engine] final def isPendingAndNotCancelled: Boolean = Bits.isPendingAndNotCancelled(varyingBits)
  private[engine] final def isCancellationUnlatched: Boolean = Bits.isCancellationUnlatched(varyingBits)


  inline protected final def setCompletionToSuccess(): Unit =
    varyingBits = (varyingBits | Bits.Completion_Success_bug).toByte

  //// meaningful only in FiberImpl, but moved here for convenience
  inline protected final def setCancellationLatch(): Unit =
    varyingBits = (varyingBits | Bits.Cancellation_Latch_Bug).toByte


  //-------------------------------------------------------------------
  // atomically
  //-------------------------------------------------------------------


  inline protected final def spinAcquire(): Boolean = casVH(false, true)
  inline protected final def spinRelease(): Unit = setVH(false)


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
    waiter.theWaiteeOrBlocker = this


  //// Simpler `subscribeWaiterUnsync`, for when the caller knows that waiter list is empty
  final def subscribeFirstWaiterUnsync(waiter: FiberImpl): Unit =
    firstWaiter = waiter
    waiter.linkWaiterWithSelf()
    waiter.theWaiteeOrBlocker = this


  final def unsubscribeWaiter(waiter: FiberImpl): Unit =
    val willResume =
      atomically {
        if isPending then
          if waiter.theWaiteeOrBlocker != null then
            //// assert(waiter.theWaiteeOrBlocker == this)
            removeWaiterAnywhere(waiter)
            waiter.clearWaiterLink()
            waiter.theWaiteeOrBlocker = null
            true
          else
            false
        else
          false
      }

    if willResume then
      waiter.resume()

    afterUnsubscribe()


  //// Currently only needed by Semaphore
  protected def afterUnsubscribe(): Unit = ()    


  final def finallyNotifyAllWaiters(): Unit =
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


  private final def removeWaiterAnywhere(waiter: FiberImpl): Unit =
    if waiter.isWaiterLinkedWithSelf then
      firstWaiter = null
    else
      if waiter == firstWaiter then
        firstWaiter = waiter.nextWaiter.nn.asFiber
      waiter.removeWaiterAtSelf()


private[concurrent] object Waitee:
  private def notifyAllWaiters(first: FiberImpl): Unit =
    @tailrec def loop(waiter: FiberImpl): Unit =
      val next = waiter.nextWaiter.nn.asFiber
      waiter.resume()
      if next != first then
        loop(next)
    loop(first)

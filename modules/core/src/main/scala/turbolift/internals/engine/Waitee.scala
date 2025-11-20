package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.internals.engine.concurrent.atomic.AtomicBoolVH


/** Either Fiber, Warp, Queque or anything from `turbolift.internals.engine.concurrent.*` */

private[engine] abstract class Waitee extends AtomicBoolVH(false):
  protected var theFirstWaiter: FiberImpl | Null = null
  protected var varyingBits: Byte = 0


  //-------------------------------------------------------------------
  // bits
  //-------------------------------------------------------------------


  private[engine] final def isPending: Boolean = Bits.isPending(varyingBits)
  private[engine] final def isCancellationSignalled: Boolean = Bits.isCancellationSignalled(varyingBits)
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


  //// Assumes it's called (indirectly) from the `fiber` itself.
  inline final def atomicallyBoth(fiber: FiberImpl)(inline body: => Halt): Halt =
    if spinAcquireBoth(fiber) then
      val a = body
      spinReleaseBoth(fiber)
      a
    else
      Halt.Cancel


  final private def spinAcquireBoth(fiber: FiberImpl): Boolean =
    if fiber.theCurrentEnv.isCancellable then
      spinAcquireBothCancellable(fiber)
    else
      spinAcquireBothUncancellable(fiber)
      true


  inline final private def spinAcquireBothCancellable(fiber: FiberImpl): Boolean =
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


  inline final private def spinAcquireBothUncancellable(fiber: FiberImpl): Unit =
    @tailrec def loop(): Unit =
      if fiber.spinAcquire() then
        if spinAcquire() then
          ()
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
    val x = theFirstWaiter
    if x == null then
      theFirstWaiter = waiter
      waiter.linkWaiterWithSelf()
    else
      x.insertWaiterBeforeSelf(waiter)
    waiter.theWaiteeOrBlocker = this


  //// Simpler `subscribeWaiterUnsync`, for when the caller knows that waiter list is empty
  final def subscribeFirstWaiterUnsync(waiter: FiberImpl): Unit =
    theFirstWaiter = waiter
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
    val x = theFirstWaiter
    if x != null then
      theFirstWaiter = null
      Waitee.notifyAllWaiters(x)


  final def removeFirstWaiter(): Unit =
    val x = theFirstWaiter.nn
    val y = x.theNextWaiter
    if x == y then
      theFirstWaiter = null
    else
      theFirstWaiter = y.nn.asFiber
    x.removeWaiterAtSelf()


  private final def removeWaiterAnywhere(waiter: FiberImpl): Unit =
    if waiter.isWaiterLinkedWithSelf then
      theFirstWaiter = null
    else
      if waiter == theFirstWaiter then
        theFirstWaiter = waiter.theNextWaiter.nn.asFiber
      waiter.removeWaiterAtSelf()


private[engine] object Waitee:
  private def notifyAllWaiters(first: FiberImpl): Unit =
    @tailrec def loop(waiter: FiberImpl): Unit =
      val next = waiter.theNextWaiter.nn.asFiber
      waiter.resume()
      if next != first then
        loop(next)
    loop(first)

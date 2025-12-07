package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.io.{Fiber, Warp}


private[turbolift] final class WarpImpl private[engine] (
  _parent: ChildLink | Null,
  private val theOuter: WarpImpl | Null,
  private var theName: String,
  val exitMode: Warp.ExitMode | Null,
) extends ChildLink(_parent) with Warp.Unsealed:
  private var theFiberCount: Int = 0
  private var theWarpCount: Int = 0

  private def isChildless: Boolean = (theFiberCount == 0) && (theWarpCount == 0)
  private def isShutdown: Boolean = theCancellation == Bits.Cancellation_Latched


  //-------------------------------------------------------------------
  // Add & Remove Child
  //-------------------------------------------------------------------


  def tryAddFiber(fiber: FiberImpl): Boolean =
    atomically {
      //// If cancelled, do not modify child list, bcoz `deepCancelLoop` may be concurrently running.
      if isPendingAndNotCancelled then
        insertLastChild(fiber)
        //// the only difference tryAddFiber & tryAddWarp:
        this.theFiberCount += 1
        true
      else
        false
    }

  def tryAddWarp(warp: WarpImpl): Boolean =
    atomically {
      //// If cancelled, do not modify child list, bcoz `deepCancelLoop` may be concurrently running.
      if isPendingAndNotCancelled then
        insertLastChild(warp)
        //// the only difference tryAddFiber & tryAddWarp:
        this.theWarpCount += 1
        true
      else
        false
    }


  //// The mutual loop of removeFiber/Child + doFinalize could be rewritten as @tailrec
  //// ...but inlining `doFinalize` causes compiler problems.

  def removeFiber(fiber: FiberImpl): Unit =
    val willFinalize =
      atomically {
        //// If cancelled, do not modify child list, bcoz `deepCancelLoop` may be concurrently running.
        if isPendingAndNotCancelled then
          removeChildAnywhere(fiber)
        //// the only difference removeFiber & removeWarp:
        this.theFiberCount -= 1
        shutdownCheck()
      }

    if willFinalize then
      doFinalize()


  def removeWarp(warp: WarpImpl): Unit =
    val willFinalize =
      atomically {
        //// If cancelled, do not modify child list, bcoz `deepCancelLoop` may be concurrently running.
        if isPendingAndNotCancelled then
          removeChildAnywhere(warp)
        //// the only difference removeFiber & removeWarp:
        this.theWarpCount -= 1
        shutdownCheck()
      }

    if willFinalize then
      doFinalize()


  private inline def shutdownCheck(): Boolean =
    if isShutdown & isChildless then
      this.theCompletion = true
      true
    else
      false


  //-------------------------------------------------------------------
  // Await & Shutdown
  //-------------------------------------------------------------------


  def intrinsicAwait(waiter: FiberImpl): Halt =
    waiter.willContinuePure(())
    awaitBy(waiter)


  def awaitBy(waiter: FiberImpl): Halt =
    var willFinalize = false

    val result =
      atomicallyBoth(waiter) {
        if isPending then
          if isChildless then
            this.theCompletion = true
            willFinalize = true
            Halt.Continue
          else
            this.theCancellation = Bits.Cancellation_Latched
            subscribeWaiterUnsync(waiter)
            Halt.Retire
        else
          Halt.Continue
      }

    if willFinalize then
      doFinalize()
    result


  //// Same as `awaitBy(waiter)`, except:
  //// - doesn't synchronize on the `waiter`
  //// - doesn't subscribe the `waiter`
  //// - returns Unit, instead of `Halt`
  def doShutdownAndForget(): Unit =
    val willFinalize =
      atomically {
        if isPending then
          if isChildless then
            this.theCompletion = true
            true
          else
            this.theCancellation = Bits.Cancellation_Latched
            false
        else
          false
      }

    if willFinalize then
      doFinalize()


  //-------------------------------------------------------------------
  // Cancelling
  //-------------------------------------------------------------------


  def intrinsicCancel(canceller: FiberImpl): Halt =
    canceller.willContinuePure(())
    cancelBy(canceller)


  def cancelBy(canceller: FiberImpl): Halt =
    var willFinalize = false
    var willDescend = false

    val halt =
      atomicallyBoth(canceller) {
        if isPending then
          if isChildless then
            this.theCompletion = true
            willFinalize = true
            Halt.Continue
          else
            if !isCancellationSignalled then
              this.theCancellation = Bits.Cancellation_Latched
              willDescend = true
            subscribeWaiterUnsync(canceller)
            Halt.Retire
        else
          Halt.Continue
      }

    if willFinalize then
      doFinalize()
    else
      if willDescend then
        doDescend(deep = true)
    halt


  //// Same as `cancelBy`, except:
  //// - doesn't synchronize on the `canceller`
  //// - doesn't subscribe the `canceller`
  //// - doesn't initiate `deepCancelLoop`
  //// - returns first child, instead of `Halt`
  private[engine] override def shallowCancel(): ChildLink | Null =
    var willFinalize = false
    var willDescend = false

    atomically {
      if isPending then
        if isChildless then
          this.theCompletion = true
          willFinalize = true
        else
          if !isCancellationSignalled then
            this.theCancellation = Bits.Cancellation_Latched
            willDescend = true
    }

    if willFinalize then
      doFinalize()
      null
    else
      if willDescend then
        doDescend(deep = false)
      else
        null


  //-------------------------------------------------------------------
  // Finalize & Descend
  //-------------------------------------------------------------------


  private def doFinalize(): Unit =
    finallyNotifyAllWaiters()

    getParent match
      case warp: WarpImpl => warp.removeWarp(this)
      case _ => ()


  private def doDescend(deep: Boolean): ChildLink | Null =
    val firstChild = getFirstChild
    if firstChild != null then
      firstChild.markAsFirstChild()
      if deep then
        firstChild.deepCancelLoop(this)
    firstChild


  //-------------------------------------------------------------------
  // Public API
  //-------------------------------------------------------------------


  override def name: String =
    if theName.isEmpty then
      theName = s"Warp#%04X".format(hashCode & 0xFFFF)
    theName


  override def toString: String = name
  override def parent: Option[Warp | Fiber.Untyped] = if getParent == null then None else Some(getParent.asInstanceOf[Warp | Fiber.Untyped])
  override def outer: Option[Warp] = if theOuter == null then None else Some(theOuter)
  override def unsafeChildren(): Iterable[Fiber.Untyped | Warp] = collectChildren(_ => true)
  override def unsafeFibers(): Iterable[FiberImpl] = collectChildren(_.isInstanceOf[FiberImpl])
  override def unsafeWarps(): Iterable[Warp] = collectChildren(_.isInstanceOf[WarpImpl])

  //// Limitation: when the warp is cancelled but not yet completed,
  //// it will report empty child-LIST, even though child-COUNT is >0.
  //@#@TODO old stuff
  // private def collectChildren[T <: ChildLink](filter: ChildLink => Boolean): Iterable[T]


  override def cancel: Unit !! IO = CC.intrinsic(intrinsicCancel(_))
  override def await: Unit !! IO = CC.intrinsic(intrinsicAwait(_))
  override def unsafeShutdownAndForget(): Unit = doShutdownAndForget()
  override def unsafeCancelAndForget(): Unit = doCancelAndForget()


  override def unsafeSpawn(name: String): Warp =
    val child = new WarpImpl(this, null, name, null)
    if !tryAddWarp(child) then
      this.theCompletion = true
    child


  override def unsafeStatus(): Warp.Status =
    var savedCompletion: Boolean = false
    var savedCancellation: Byte = 0
    var savedFiberCount: Int = 0
    var savedWarpCount: Int = 0

    atomically {
      savedCompletion = theCompletion
      savedCancellation = theCancellation
      savedFiberCount = theFiberCount
      savedWarpCount = theWarpCount
    }
    if savedCompletion then
      Warp.Status.Completed
    else
      Warp.Status.Pending(
        fiberCount = savedFiberCount,
        warpCount = savedWarpCount,
        isShutdown = savedCancellation == Bits.Cancellation_Latched,
        isCancelled = savedCancellation >= Bits.Cancellation_Signalled,
      )


private[turbolift] object WarpImpl:
  val root: WarpImpl = new WarpImpl(null, null, "RootWarp", null)

package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.io.{Fiber, Warp}


private[turbolift] final class WarpImpl private[engine] (
  private val theParent: WarpImpl | Null,
  private var theName: String,
  val exitMode: Warp.ExitMode | Null,
) extends ChildLink with Warp.Unsealed:
  private var packedChildCount: Long = 0
  private var firstChild: ChildLink | Null = null
  private var callback: (() => Unit) | Null = null
  // protected[this] val pad1 = 0


  def initMain(cb: () => Unit): Unit =
    callback = cb
    varyingBits = (varyingBits | Bits.Warp_Shutdown).toByte


  private def isChildless: Boolean = packedChildCount == 0L
  private def isShutdown: Boolean = Bits.isShutdown(varyingBits)


  //-------------------------------------------------------------------
  // Add & Remove Child
  //-------------------------------------------------------------------


  def tryAddFiber(fiber: FiberImpl): Boolean = tryAddChild(fiber, WarpImpl.ONE_FIBER)
  def tryAddWarp(warp: WarpImpl): Boolean = tryAddChild(warp, WarpImpl.ONE_WARP)
  def removeFiber(fiber: FiberImpl): Unit = removeChild(fiber, WarpImpl.ONE_FIBER)
  def removeWarp(warp: WarpImpl): Unit = removeChild(warp, WarpImpl.ONE_WARP)


  private def tryAddChild(child: ChildLink, oneCount: Long): Boolean =
    atomically {
      //// If cancelled, do not modify child list, bcoz `deepCancelLoop` may be concurrently running.
      if isPendingAndNotCancelled then
        val x = firstChild
        if x == null then
          firstChild = child
          child.linkChildWithSelf()
        else
          x.insertChildBeforeSelf(child)

        packedChildCount += oneCount
        true
      else
        false
    }


  //// Should be @tailrec, but inlining `doFinalize` causes problems
  private def removeChild(child: ChildLink, oneCount: Long): Unit =
    val willFinalize =
      atomically {
        //// If cancelled, do not modify child list, bcoz `deepCancelLoop` may be concurrently running.
        if isPendingAndNotCancelled then
          if child.isChildLinkedWithSelf then
            firstChild = null
          else
            if child == firstChild then
              firstChild = child.nextChild
            child.removeChildAtSelf()
          child.clearChildLink()

        packedChildCount -= oneCount
        if (packedChildCount == 0L) & isShutdown then
          varyingBits = (varyingBits | Bits.Warp_Completed).toByte
          true
        else
          false
      }

    if willFinalize then
      doFinalize()


  //-------------------------------------------------------------------
  // Await & Shutdown
  //-------------------------------------------------------------------


  def tryGetAwaitedBy(waiter: FiberImpl): Int =
    var willFinalize = false

    val result =
      atomicallyIfNotCancelled(waiter) {
        if isPending then
          if isChildless then
            varyingBits = (varyingBits | Bits.Warp_Completed).toByte
            willFinalize = true
            Bits.WaiteeAlreadyCompleted
          else
            varyingBits = (varyingBits | Bits.Warp_Shutdown).toByte
            subscribeWaiterUnsync(waiter)
            Bits.WaiterSubscribed
        else
          Bits.WaiteeAlreadyCompleted
      }

    if willFinalize then
      doFinalize()
    result


  //// Same as `tryGetAwaitedBy(waiter)`, except:
  //// - doesn't synchronize on the `waiter`
  //// - doesn't subscribe the `waiter`
  //// - returns Unit, instead of Int code
  def doShutdownAndForget(): Unit =
    val willFinalize =
      atomically {
        if isPending then
          if isChildless then
            varyingBits = (varyingBits | Bits.Warp_Completed).toByte
            true
          else
            varyingBits = (varyingBits | Bits.Warp_Shutdown).toByte
            false
        else
          false
      }

    if willFinalize then
      doFinalize()


  //-------------------------------------------------------------------
  // Cancelling
  //-------------------------------------------------------------------


  def tryGetCancelledBy(canceller: FiberImpl): Int =
    var willFinalize = false
    var willDescend = false

    val result =
      atomicallyIfNotCancelled(canceller) {
        if isPending then
          if isChildless then
            varyingBits = (varyingBits | Bits.Warp_Completed).toByte
            willFinalize = true
            Bits.WaiteeAlreadyCompleted
          else
            if !isCancelled then
              varyingBits = (varyingBits | Bits.Warp_Shutdown | Bits.Warp_Cancelled).toByte
              willDescend = true
            subscribeWaiterUnsync(canceller)
            Bits.WaiterSubscribed
        else
          Bits.WaiteeAlreadyCompleted
      }

    if willFinalize then
      doFinalize()
    else
      if willDescend then
        doDescend(deep = true)
    result



  //// Same as `tryGetCancelledBy`, except:
  //// - doesn't synchronize on the `canceller`
  //// - doesn't subscribe the `canceller`
  //// - doesn't initiate `deepCancelLoop`
  //// - returns first child, instead of Int code
  private[engine] override def deepCancelDown(): ChildLink | Null =
    var willFinalize = false
    var willDescend = false

    atomically {
      if isPending then
        if isChildless then
          varyingBits = (varyingBits | Bits.Warp_Completed).toByte
          willFinalize = true
        else
          if !isCancelled then
            varyingBits = (varyingBits | Bits.Warp_Shutdown | Bits.Warp_Cancelled).toByte
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


  private[engine] override def deepCancelRight(): ChildLink | Null = nextChild

  private[engine] override def deepCancelUp(): ChildLink = theParent.nn


  //-------------------------------------------------------------------
  // Finalize & Descend
  //-------------------------------------------------------------------


  private def doFinalize(): Unit =
    notifyAllWaiters()

    if theParent != null then
      theParent.nn.removeWarp(this)

    if callback != null then
      callback.nn()


  private def doDescend(deep: Boolean): ChildLink | Null =
    val x = firstChild
    if x != null then
      firstChild = null

      //// Break the cycle to mark the end of list
      x.prevChild.nn.nextChild = null

      if deep then
        x.deepCancelLoop(this)
    x


  //-------------------------------------------------------------------
  // Public API
  //-------------------------------------------------------------------


  override def name: String =
    if theName.isEmpty then
      theName = s"Warp#%04X".format(hashCode & 0xFFFF)
    theName


  override def toString: String = name
  override def isRoot: Boolean = theParent == null
  override def parent: Warp = if theParent != null then theParent else this
  override def unsafeChildren(): Iterable[Fiber.Untyped | Warp] = collectChildren(_ => true)
  override def unsafeFibers(): Iterable[FiberImpl] = collectChildren(_.isInstanceOf[FiberImpl])
  override def unsafeWarps(): Iterable[Warp] = collectChildren(_.isInstanceOf[WarpImpl])


  //// Limitation: when the warp is cancelled but not yet completed,
  //// it will report empty child-LIST, even though child-COUNT is >0.
  private def collectChildren[T <: ChildLink](filter: ChildLink => Boolean): Iterable[T] =
    var array: Array[ChildLink] = Array.empty

    @tailrec def loop(todo: ChildLink, limit: ChildLink, index: Int): Int =
      val index2 =
        if filter(todo) then
          array(index) = todo
          index + 1
        else
          index
      val more = todo.nextChild.nn
      if more eq limit then
        index2
      else
        loop(more, limit, index2)

    val count =
      atomically {
        val x = firstChild
        if isPending && (x != null) then
          array = new Array[ChildLink](WarpImpl.unpackChildCount(packedChildCount))
          loop(x, x, 0)
        else
          0
      }

    array.asInstanceOf[Array[T]].take(count)


  override def unsafeShutdownAndForget(): Unit = doShutdownAndForget()
  override def unsafeCancelAndForget(): Unit = doCancelAndForget()


  override def unsafeSpawn(name: String): Warp =
    val child = new WarpImpl(this, name, null)
    if !tryAddWarp(child) then
      child.varyingBits = Bits.Warp_Completed
    child


  override def unsafeStatus(): Warp.Status =
    var savedVaryingBits: Byte = 0
    var savedPackedChildCount: Long = 0L
    atomically {
      savedVaryingBits = varyingBits
      savedPackedChildCount = packedChildCount
    }
    if Bits.isPending(savedVaryingBits) then
      Warp.Status.Pending(
        fiberCount = WarpImpl.unpackFiberCount(savedPackedChildCount),
        warpCount = WarpImpl.unpackWarpCount(savedPackedChildCount),
        isShutdown = Bits.isShutdown(savedVaryingBits),
        isCancelled = Bits.isCancellationSignalled(savedVaryingBits),
      )
    else
      Warp.Status.Completed


private[turbolift] object WarpImpl:
  val root: WarpImpl = new WarpImpl(null, "RootWarp", null)
  
  private[engine] def initial(): WarpImpl =
    val warp = new WarpImpl(root, "", Warp.ExitMode.Cancel)
    warp.theName = "Init" + warp.name
    root.tryAddWarp(warp)
    warp

  private inline val ONE_FIBER = 1L
  private inline val ONE_WARP = 1L << 32
  private def unpackFiberCount(n: Long): Int = n.toInt
  private def unpackWarpCount(n: Long): Int = (n >> 32).toInt
  private def unpackChildCount(n: Long): Int = unpackFiberCount(n) + unpackWarpCount(n)

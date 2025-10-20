package turbolift.internals.engine
import scala.annotation.tailrec


/** Either Fiber or Warp. */

private abstract class ChildLink(
  private val theParent: ChildLink | Null
) extends WaiterLink:
  private var theFirstChild: ChildLink | Null = null
  private var thePrevSibling: ChildLink | Null = null
  private var theNextSibling: ChildLink | Null = null
  private var theFirstSiblingMarker: Boolean = false


  //-------------------------------------------------------------------
  // aux
  //-------------------------------------------------------------------


  private final inline def insertSiblingBeforeSelf(that: ChildLink): Unit =
    val prev = thePrevSibling.nn
    prev.linkSiblingWith(that)
    that.linkSiblingWith(this)

  private final inline def removeSelfFromSiblings(): Unit =
    val prev = thePrevSibling.nn
    val next = theNextSibling.nn
    prev.linkSiblingWith(next)

  private final inline def linkSiblingWith(that: ChildLink): Unit =
    this.theNextSibling = that
    that.thePrevSibling = this

  private final inline def clearSiblingLink(): Unit =
    this.thePrevSibling = null
    this.theNextSibling = null

  private final inline def linkSiblingWithSelf(): Unit = linkSiblingWith(this)


  //-------------------------------------------------------------------
  // public
  //-------------------------------------------------------------------


  //// Callable only from `atomically` blocks, or on a fresh Fiber/Warp
  final def insertLastChild(child: ChildLink): Unit =
    val that = theFirstChild
    if that == null then
      this.theFirstChild = child
      child.linkSiblingWithSelf()
    else
      that.insertSiblingBeforeSelf(child)


  //// Callable only from `atomically` blocks, or on a fresh Fiber/Warp
  final def removeChildAnywhere(child: ChildLink): Unit =
    val sibling = child.theNextSibling
    if child == sibling then
      //// child is the only child
      this.theFirstChild = null
    else
      if child == theFirstChild then
        this.theFirstChild = sibling
      child.removeSelfFromSiblings()
    child.clearSiblingLink()


  //// Callable only from `atomically` blocks, or on a fresh Fiber/Warp
  final def emptyInsertTwoChildren(left: ChildLink, right: ChildLink): Unit =
    this.theFirstChild = left
    left.theFirstSiblingMarker = true
    left.linkSiblingWith(right)
    right.linkSiblingWith(left)


  //// Callable only from `atomically` blocks, or on a fresh Fiber/Warp
  final def emptyInsertOneChild(child: ChildLink): Unit =
    this.theFirstChild = child
    child.theFirstSiblingMarker = true
    child.linkSiblingWithSelf()


  final def clearChildren(): Unit =
    this.theFirstChild = null


  final def collectChildren[T <: ChildLink](filter: ChildLink => Boolean): Array[T] =
    val builder = scala.collection.mutable.ArrayBuilder.make[ChildLink]

    @tailrec def loop(todo: ChildLink, limit: ChildLink): Unit =
      if filter(todo) then
        builder += todo
      val more = todo.theNextSibling.nn
      if more ne limit then
        loop(more, limit)

    atomically {
      val x = theFirstChild
      if isPending && (x != null) then
        loop(x, x)
    }

    builder.result().asInstanceOf[Array[T]]


  final def getFirstChild: ChildLink | Null = theFirstChild
  final def getNextSibling: ChildLink | Null = theNextSibling
  final def getParent: ChildLink | Null = theParent


  //// Used only by Warp.
  //// Fiber's doesn't need that, because its child list is immutable, so the mark is put at start of race.
  //// Warps's child list starts as mutable, but becomes immutable on `shutdown` or `cancel`
  final inline def markAsFirstChild(): Unit = theFirstSiblingMarker = true


  //-------------------------------------------------------------------
  // Deep Cancel Loop
  //-------------------------------------------------------------------


  private[engine] final def doCancelAndForget(): Unit =
    val child = shallowCancel()
    if child != null then
      child.deepCancelLoop(this)


  //// Called only from `deepCancelLoop`
  //// Returns the first child IF cancellation signal was not set yet
  private[engine] def shallowCancel(): ChildLink | Null


  //// Recursively cancel all children of `initial`, in "fire & forget" way (no awaiting for completion).
  @tailrec private[engine] final def deepCancelLoop(initial: ChildLink, backtracking: Boolean = false): Unit =
    //// If first time here, begin visiting children: cancel self and then return first child
    val child =
      if !backtracking then
        shallowCancel()
      else
        null

    if child != null then
      //// Descend to first child
      child.deepCancelLoop(initial)
    else
      //// Either has no children, or has already visited all children and backtracked up here. Proceed to siblings.
      if this == initial then
        () //// Ignore siblings of initial. End of loop.
      else
        val sibling = getNextSibling.nn
        if !sibling.theFirstSiblingMarker then
          sibling.deepCancelLoop(initial)
        else
          //// Either has no siblings, or has already visited all siblings. Backtrack to parent.
          val parent = theParent.nn
          parent.deepCancelLoop(initial, backtracking = true)
          // if parent == initial then
          //   () //// End of loop.
          // else
          //   parent.deepCancelLoop(initial, backtracking = true)

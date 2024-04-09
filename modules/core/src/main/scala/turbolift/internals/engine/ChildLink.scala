package turbolift.internals.engine
import scala.annotation.tailrec


/** Either Fiber or Warp. */

private abstract class ChildLink extends WaiterLink:
  private[engine] var prevChild: ChildLink | Null = null
  private[engine] var nextChild: ChildLink | Null = null

  private[engine] final def insertChildBeforeSelf(that: ChildLink): Unit =
    val prev = prevChild.nn
    prev.linkChildWith(that)
    that.linkChildWith(this)

  private[engine] final def removeChildAtSelf(): Unit =
    val prev = prevChild.nn
    val next = nextChild.nn
    prev.linkChildWith(next)

  private final inline def linkChildWith(that: ChildLink): Unit =
    this.nextChild = that
    that.prevChild = this

  private[engine] final inline def clearChildLink(): Unit =
    this.prevChild = null
    this.nextChild = null

  private[engine] final def linkChildWithSelf(): Unit = linkChildWith(this)
  private[engine] final def isChildLinkedWithSelf: Boolean = isChildLinkedWith(this)
  private[engine] final def isChildLinkedWith(that: ChildLink): Boolean = this.nextChild == that


  //-------------------------------------------------------------------
  // Cancelling
  //-------------------------------------------------------------------


  private[engine] final def doCancelAndForget(): Unit =
    val child = deepCancelDown()
    if child != null then
      child.deepCancelLoop(this)


  //// Called only from `deepCancelLoop`
  private[engine] def deepCancelDown(): ChildLink | Null
  private[engine] def deepCancelRight(): ChildLink | Null
  private[engine] def deepCancelUp(): ChildLink


  //// Recursively cancel all children of `initial`, in "fire & forget" way (no awaiting for completion).
  @tailrec private[engine] final def deepCancelLoop(initial: ChildLink, backtracking: Boolean = false): Unit =
    //// If first time here, begin visiting children: cancel self and then return first child
    val child =
      if !backtracking then
        deepCancelDown()
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
        val sibling = deepCancelRight()
        if sibling != null then
          sibling.deepCancelLoop(initial)
        else
          //// Either has no siblings, or has already visited all siblings. Backtrack to parent.
          val parent = deepCancelUp()
          parent.deepCancelLoop(initial, backtracking = true)
          // if parent == initial then
          //   () //// End of loop.
          // else
          //   parent.deepCancelLoop(initial, backtracking = true)

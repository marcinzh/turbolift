package turbolift.internals.engine
import scala.annotation.tailrec


/** Either Fiber, Warp or Queque */

private[engine] abstract class WaiterLink extends Waitee:
  private[engine] var thePrevWaiter: WaiterLink | Null = null
  private[engine] var theNextWaiter: WaiterLink | Null = null

  private[engine] final def insertWaiterBeforeSelf(that: WaiterLink): Unit =
    val prev = thePrevWaiter.nn
    prev.linkWaiterWith(that)
    that.linkWaiterWith(this)

  private[engine] final def removeWaiterAtSelf(): Unit =
    val prev = thePrevWaiter.nn
    val next = theNextWaiter.nn
    prev.linkWaiterWith(next)

  private final inline def linkWaiterWith(that: WaiterLink): Unit =
    this.theNextWaiter = that
    that.thePrevWaiter = this

  private[engine] final inline def clearWaiterLink(): Unit =
    this.thePrevWaiter = null
    this.theNextWaiter = null

  private[engine] final def linkWaiterWithSelf(): Unit = linkWaiterWith(this)
  private[engine] final def isWaiterLinkedWithSelf: Boolean = isWaiterLinkedWith(this)
  private[engine] final def isWaiterLinkedWith(that: WaiterLink): Boolean = this.theNextWaiter == that


  //-------------------------------------------------------------------
  // WaiterLink specific (no counterparts in ChildLink)
  //-------------------------------------------------------------------


  private[engine] final def removeWaiterAfterSelf(): WaiterLink =
    val firstNext = theNextWaiter.nn
    val secondNext = firstNext.theNextWaiter.nn
    linkWaiterWith(secondNext)
    firstNext.clearWaiterLink()
    firstNext


  private[engine] final def removeRangeOfWaitersAtSelf(last: WaiterLink): Unit =
    val oldLast = thePrevWaiter.nn
    val newFirst = last.theNextWaiter.nn
    oldLast.linkWaiterWith(newFirst)


  //@#@ unused
  private[engine] final def appendManyWaiters(that: WaiterLink): Unit =
    val leftFirst = this
    val leftLast = this.thePrevWaiter.nn
    val rightFirst = that
    val rightLast = that.thePrevWaiter.nn
    leftLast.linkWaiterWith(rightFirst)
    rightLast.linkWaiterWith(leftFirst)

  //@#@ unused
  private[engine] final def assignManyWaiters(first: WaiterLink, last: WaiterLink): Unit =
    val leftFirst = this
    val leftLast = this
    val rightFirst = first
    val rightLast = last
    leftLast.linkWaiterWith(rightFirst)
    rightLast.linkWaiterWith(leftFirst)


  private[engine] final def asFiber: FiberImpl = asInstanceOf[FiberImpl]
  private[engine] final def asWarp: WarpImpl = asInstanceOf[WarpImpl]


private[internals] object WaiterLink:
  class Queue extends WaiterLink:
    {
      linkWaiterWithSelf()
    }

    protected final def isEmpty: Boolean = isWaiterLinkedWithSelf
    protected final def enqueue(fiber: FiberImpl): Unit = insertWaiterBeforeSelf(fiber)
    protected final def enqueueMany(fiber: FiberImpl): Unit = appendManyWaiters(fiber)
    protected final def dequeue(): FiberImpl = removeWaiterAfterSelf().asFiber

    //@#@ unused
    protected final def dropAndAssingWaiters(that: WaiterLink, maxCount: Int): Int =
      assert(isEmpty)
      def loop(curr: WaiterLink, accum: Int): Int =
        if curr == that then
          accum
        else if accum == maxCount then
          assignManyWaiters(curr, that.thePrevWaiter.nn)
          accum
        else
          loop(curr.theNextWaiter.nn, accum + 1)
      loop(that.theNextWaiter.nn, 1)

package turbolift.internals.engine.concurrent
import scala.annotation.tailrec


/** Either Fiber, Warp or Queque */

private[engine] abstract class WaiterLink extends Waitee:
  private[engine] var prevWaiter: WaiterLink | Null = null
  private[engine] var nextWaiter: WaiterLink | Null = null

  private[engine] final def insertWaiterBeforeSelf(that: WaiterLink): Unit =
    val prev = prevWaiter.nn
    prev.linkWaiterWith(that)
    that.linkWaiterWith(this)

  private[engine] final def removeWaiterAtSelf(): Unit =
    val prev = prevWaiter.nn
    val next = nextWaiter.nn
    prev.linkWaiterWith(next)

  private final inline def linkWaiterWith(that: WaiterLink): Unit =
    this.nextWaiter = that
    that.prevWaiter = this

  private[engine] final inline def clearWaiterLink(): Unit =
    this.prevWaiter = null
    this.nextWaiter = null

  private[engine] final def linkWaiterWithSelf(): Unit = linkWaiterWith(this)
  private[engine] final def isWaiterLinkedWithSelf: Boolean = isWaiterLinkedWith(this)
  private[engine] final def isWaiterLinkedWith(that: WaiterLink): Boolean = this.nextWaiter == that


  //-------------------------------------------------------------------
  // WaiterLink specific (no counterparts in ChildLink)
  //-------------------------------------------------------------------


  private[engine] final def removeWaiterAfterSelf(): WaiterLink =
    val firstNext = nextWaiter.nn
    val secondNext = firstNext.nextWaiter.nn
    linkWaiterWith(secondNext)
    firstNext.clearWaiterLink()
    firstNext

  //@#@ unused
  private[engine] final def appendManyWaiters(that: WaiterLink): Unit =
    val leftFirst = this
    val leftLast = this.prevWaiter.nn
    val rightFirst = that
    val rightLast = that.prevWaiter.nn
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
          assignManyWaiters(curr, that.prevWaiter.nn)
          accum
        else
          loop(curr.nextWaiter.nn, accum + 1)
      loop(that.nextWaiter.nn, 1)

package turbolift.internals.engine


private[internals] abstract class Link:
  protected var linkLeft: Link | Null = null
  protected var linkRight: Link | Null = null

  final def toFiber: FiberImpl = asInstanceOf[FiberImpl]

  final def insertLast(that: FiberImpl): Unit =
    val last = this.linkLeft.nn
    last.linkWith(that)
    that.linkWith(this)

  final def removeFirst(): FiberImpl =
    val first = this.linkRight.nn
    val second = first.linkRight.nn
    this.linkWith(second)
    first.clearLinks()
    first.toFiber

  final def removeAnywhere(that: FiberImpl): Unit =
    val prev = that.linkLeft.nn
    val next = that.linkRight.nn
    prev.linkWith(next)
    that.clearLinks()

  final inline def linkWith(that: Link): Unit =
    this.linkRight = that
    that.linkLeft = this

  final def clearLinks(): Unit =
    this.linkLeft  = null
    this.linkRight = null

  final def linkWithSelf(): Unit = linkWith(this)
  final def isLinkedWithSelf: Boolean = isLinkedWith(this)
  final def isLinkedWith(that: Link): Boolean = this.linkRight == that


private[internals] object Link:
  class Queue extends Link:
    {
      linkWithSelf()
    }

    final def isEmpty: Boolean = isLinkedWithSelf
    final def enqueue(fiber: FiberImpl): Unit = insertLast(fiber)
    final def dequeue(): FiberImpl = removeFirst().toFiber

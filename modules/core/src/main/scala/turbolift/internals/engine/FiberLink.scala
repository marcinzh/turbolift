package turbolift.internals.engine
// import java.util.concurrent.atomic.AtomicInteger


// private[internals] abstract class FiberLink extends AtomicInteger:
private[internals] abstract class FiberLink:
  protected var linkLeft: FiberLink | Null = null
  protected var linkRight: FiberLink | Null = null

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

  final inline def linkWith(that: FiberLink): Unit =
    this.linkRight = that
    that.linkLeft = this

  final def clearLinks(): Unit =
    this.linkLeft  = null
    this.linkRight = null

  final def linkWithSelf(): Unit = linkWith(this)
  final def isLinkedWithSelf: Boolean = isLinkedWith(this)
  final def isLinkedWith(that: FiberLink): Boolean = this.linkRight == that

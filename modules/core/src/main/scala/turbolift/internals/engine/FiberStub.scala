package turbolift.internals.engine
// import java.util.concurrent.atomic.AtomicInteger
import turbolift.internals.launcher.Callback
import scala.annotation.tailrec


private[engine] abstract class FiberStub(
  protected val constantBits: Int,
  protected val parent: Fiber,
  protected val callback: Callback.Untyped,
// ) extends AtomicInteger:
):
  @volatile private var varyingBits: Int = 0

  protected var childLeft: Fiber | Null = null
  protected var childRight: Fiber | Null = null

  protected final def clearChildren(): Unit =
    this.childLeft  = null
    this.childRight = null

  protected final def findCallback: Callback.Untyped =
    constantBits match
      case Bits.Tree_Root => callback
      case _ => parent.findCallback

  protected final def whichChildAmI: Int = (this.constantBits >>> Bits.Self_Shift) & Bits.Child_Mask
  protected final def getSiblingOfChild(whichChild: Int): Fiber | Null = getChild(whichChild ^ Bits.Child_Mask)
  protected final def getSibling: Fiber | Null = parent.getSiblingOfChild(whichChildAmI)

  protected final def getChild(whichChild: Int): Fiber =
    whichChild match
      case Bits.Child_Left => childLeft.nn
      case Bits.Child_Right => childRight.nn

  final def unsafeIsCancelled(): Boolean = (this.varyingBits & Bits.Cancelled) != 0

  final def unsafeTryCancel(): Boolean =
    synchronized {
      val n = this.varyingBits
      if (n & Bits.Cancelled) == 0 then
        this.varyingBits = n | Bits.Cancelled
        true
      else
        false
    }

  protected final def makeChildrenAwaited(lhs: Fiber, rhs: Fiber): Boolean =
    synchronized {
      val n = this.varyingBits
      if n != Bits.Cancelled then
        this.varyingBits = Bits.Awaited_Both
        this.childLeft = lhs
        this.childRight = rhs
        true
      else
        false
    }

  protected final def tryWin(whichChild: Int, isSuccess: Boolean): Int =
    val successBit = if isSuccess then whichChild else 0
    val awaitedBit = whichChild << Bits.Awaited_Shift
    var fiberToCull: Fiber | Null = null
    val resultBits =
      synchronized {
        val oldBits = this.varyingBits
        if (oldBits & Bits.Awaited_ShiftedMask) == Bits.Awaited_Both then
          //// winner
          val newBits = (oldBits & ~awaitedBit) | successBit
          this.varyingBits = newBits
          if !isSuccess then
            fiberToCull = getSiblingOfChild(whichChild)
          newBits | Bits.Winner
        else
          //// loser
          val newBits = oldBits & ~(Bits.Awaited_ShiftedMask | Bits.Child_Mask)
          this.varyingBits = newBits
          (oldBits & Bits.Child_Mask) | successBit
      }
      
    if fiberToCull != null then
      fiberToCull.nn.cull()
    resultBits


  protected final def cull(): Unit = cullRec(this, Bits.Child_None)

  @tailrec final def cullRec(limit: FiberStub, comingFromChild: Int): Unit =
    val nextToVisit: FiberStub | Null = comingFromChild match
      case Bits.Child_None =>
        //// coming from parent
        synchronized {
          val n = varyingBits
          if (n & Bits.Cancelled) == 0 then
            this.varyingBits = n | Bits.Cancelled
            n & Bits.Awaited_ShiftedMask match
              case Bits.Awaited_None => null
              case Bits.Awaited_Right => childRight
              case _ => childLeft
          else
            null
        }

      case Bits.Child_Left =>
        synchronized {
          varyingBits & Bits.Awaited_ShiftedMask match
            case Bits.Child_Right => childRight
            case _ => null
        }

      case Bits.Child_Right => null

    if nextToVisit != null then
      //// go to first/next child
      nextToVisit.nn.cullRec(limit, Bits.Child_None)
    else
      //// go back to parent
      if this != limit then
        parent.cullRec(limit, whichChildAmI)

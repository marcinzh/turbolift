package turbolift.internals.engine
import scala.annotation.tailrec


private[engine] final class Frame private (
  val next: Frame | Null,
  private val packed: Int,
  val step: Step,
  val stan: Stan,
):
  assert(isGuard <= isLocal)

  private def copy(
    next: Frame | Null = next,
    packed: Int = packed,
    step: Step = step,
    stan: Stan = stan,
  ): Frame =
    new Frame(
      next = next,
      packed = packed,
      step = step,
      stan = stan,
    )


  def delta: Int = packed >> Frame.SHIFT
  def isLocal: Boolean = (packed & Frame.LOCAL) != 0
  def isGuard: Boolean = (packed & Frame.GUARD) != 0
  def isBase: Boolean = !isLocal
  def hasNext: Boolean = next != null


  def bottom: Frame = if next == null then this else next.nn.bottom
  def computeBottomHeight(initial: Int): Int =
    if next == null then initial else next.nn.computeBottomHeight(initial - delta)


  def pushNext(step: Step, stan: Stan, delta: Int, isLocal: Boolean, isGuard: Boolean): Frame =
    new Frame(
      next = this,
      packed = Frame.makePacked(delta, isLocal, isGuard),
      step = step,
      stan = stan,
    )


  def bridge: Frame =
    copy(
      next = null,
      packed = Frame.makePacked(0, isLocal, isGuard),
      step = StepCases.Bridge,
      stan = Stan.nul,
    )


  def splitLo(initialHeight: Int, divHeight: Int, oldStan: Stan): (Frame, Int, Stan) =
    @tailrec def loop(frame: Frame, height: Int, prevStan: Stan): (Frame, Int, Stan) =
      if height < divHeight then
        (frame, height, prevStan)
      else
        loop(
          frame = frame.next.nn,
          height = height - frame.delta,
          prevStan = frame.stan,
        )
    loop(
      frame = this,
      height = initialHeight,
      prevStan = oldStan,
    )


  //@#@TODO paranoid stack safety. Make `next` a var, and reverse in place
  def splitHi(initialHeight: Int, divHeight: Int): (Frame, Int, Boolean) =
    var newMinHeight = 0
    var newHasBase = false
    def loop(oldFrame: Frame, currentHeight: Int): Frame | Null =
      if currentHeight > divHeight then
        newMinHeight = currentHeight
        newHasBase = oldFrame.isBase
        val nextNewFrame = loop(oldFrame.next.nn, currentHeight - oldFrame.delta)
        copy(next = nextNewFrame)
      else
        null
    val newTopFrame = loop(this, initialHeight)
    (newTopFrame.nn, newMinHeight, newHasBase)


  override def toString: String =
    val s = s"+${delta}${step}"
    if next == null then
      s
    else
      s"$s;${next.toString}"


private[engine] object Frame:
  val base: Frame = pushFirst(StepCases.Pop, isLocal = false, isGuard = false)

  def pushFirst(step: Step, isLocal: Boolean, isGuard: Boolean): Frame =
    new Frame(
      next = null,
      packed = Frame.makePacked(0, isLocal, isGuard),
      step = step,
      stan = Stan.nul,
    )

  private[this] def makePacked(delta: Int, isLocal: Boolean, isGuard: Boolean): Int =
    (if isLocal then LOCAL else 0) |
    (if isGuard then GUARD else 0) |
    (delta << 2)

  private[this] inline val LOCAL = 0x1
  private[this] inline val GUARD = 0x2
  private[this] inline val SHIFT = 2

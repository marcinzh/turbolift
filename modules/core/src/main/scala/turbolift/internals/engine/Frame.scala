package turbolift.internals.engine
import scala.annotation.tailrec


private[engine] final class Frame private (
  val next: Frame | Null,
  private val packed: Int,
  val step: Step,
  val stan: Stan,
  val guard: AnyGuardFunc | Null,
):
  private def copy(
    next: Frame | Null = next,
    packed: Int = packed,
    step: Step = step,
    stan: Stan = stan,
    guard: AnyGuardFunc | Null = guard,
  ): Frame =
    new Frame(
      next = next,
      packed = packed,
      step = step,
      stan = stan,
      guard = guard,
    )


  def delta: Int = packed >> 1
  def isLocal: Boolean = (packed & 1) != 0
  def isBase: Boolean = !isLocal
  def hasNext: Boolean = next != null


  def bottom: Frame = if next == null then this else next.nn.bottom
  def computeBottomHeight(initial: Int): Int =
    if next == null then initial else next.nn.computeBottomHeight(initial - delta)


  def pushNext(step: Step, stan: Stan, guard: AnyGuardFunc | Null, delta: Int, isLocal: Boolean): Frame =
    new Frame(
      next = this,
      packed = Frame.makePacked(delta, isLocal),
      step = step,
      stan = stan,
      guard = guard,
    )


  def bridge: Frame =
    copy(
      next = null,
      packed = Frame.makePacked(0, isLocal),
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
  val base: Frame = pushFirst(StepCases.Pop, guard = null, isLocal = false)

  def pushFirst(step: Step, guard: AnyGuardFunc | Null, isLocal: Boolean): Frame =
    new Frame(
      next = null,
      packed = Frame.makePacked(0, isLocal),
      step = step,
      stan = Stan.nul,
      guard = guard,
    )

  private[this] def makePacked(delta: Int, isLocal: Boolean): Int =
    (delta << 1) | (if isLocal then 1 else 0) 

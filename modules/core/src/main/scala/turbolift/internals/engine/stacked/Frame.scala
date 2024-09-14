package turbolift.internals.engine.stacked
import scala.annotation.tailrec


private final class Frame private (
  val next: Frame | Null,
  private val packed: FramePacked,
  val step: Step,
  val local: Local,
):
  assert(isGuard <= isNested)

  private def copy(
    next: Frame | Null = next,
    packed: FramePacked = packed,
    step: Step = step,
    local: Local = local,
  ): Frame =
    new Frame(
      next = next,
      packed = packed,
      step = step,
      local = local,
    )


  def delta: Int = packed.delta
  def kind: FrameKind = packed.kind
  def isNested: Boolean = packed.isNested
  def isGuard: Boolean = packed.isGuard
  def isBase: Boolean = !isNested
  def hasNext: Boolean = next != null


  def bottom: Frame = if next == null then this else next.nn.bottom
  def computeBottomHeight(initial: Int): Int =
    if next == null then initial else next.nn.computeBottomHeight(initial - delta)


  def pushNext(step: Step, local: Local, delta: Int, isNested: Boolean, kind: FrameKind): Frame =
    new Frame(
      next = this,
      packed = FramePacked(delta, isNested, kind),
      step = step,
      local = local,
    )


  def bridge: Frame =
    new Frame(
      next = null,
      packed = packed.clearDelta,
      step = Step.Bridge,
      local = Local.nul,
    )


  def splitLo(initialHeight: Int, divHeight: Int, oldLocal: Local): (Frame, Int, Local) =
    @tailrec def loop(frame: Frame, height: Int, prevLocal: Local): (Frame, Int, Local) =
      if height < divHeight then
        (frame, height, prevLocal)
      else
        loop(
          frame = frame.next.nn,
          height = height - frame.delta,
          prevLocal = frame.local,
        )
    loop(
      frame = this,
      height = initialHeight,
      prevLocal = oldLocal,
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


private object Frame:
  val base: Frame = pushFirst(Step.Pop, isNested = false, FrameKind.plain)

  def pushFirst(step: Step, isNested: Boolean, kind: FrameKind): Frame =
    new Frame(
      next = null,
      packed = FramePacked(0, isNested, kind),
      step = step,
      local = Local.nul,
    )

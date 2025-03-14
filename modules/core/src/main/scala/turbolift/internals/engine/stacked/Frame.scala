package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import Frame.Split


private final class Frame private (
  private var nextVar: Frame | Null,
  private var packedVar: FramePacked,
  val step: Step,
  val local: Local,
) extends Cloneable:
  assert(isGuard <= isNested)

  def next: Frame | Null = nextVar
  def packed: FramePacked = packedVar

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
      nextVar = this,
      packedVar = FramePacked(delta, isNested, kind),
      step = step,
      local = local,
    )


  def split(initialHeight: Int, divHeight: Int): Split =
    assert(divHeight <= initialHeight)
    assert(divHeight > computeBottomHeight(initialHeight))
    assert(hasNext)
  
    def loop(prevFrame: Frame | Null, currFrame: Frame, currHeight: Int): Split =
      assert(currHeight >= divHeight)
      val nextFrame = next.nn
      val nextHeight = currHeight - currFrame.delta
      val currLocal = currFrame.local
      val currFrame2 = currFrame.clone().asInstanceOf[Frame]
      currFrame2.nextVar = prevFrame
      if nextHeight < divHeight then
        //// split between `currFrame` and `nextFrame`
        currFrame2.packedVar = currFrame2.packed.clearDelta
        Split(
          frameHi = currFrame2.reverseInPlace(),
          frameLo = nextFrame,
          heightHi = currHeight,
          heightLo = nextHeight,
          local = currLocal,
        )
      else
        loop(currFrame2, nextFrame, nextHeight)
    loop(null, this, initialHeight)


  @tailrec private def reverseInPlace(last: Frame | Null = null): Frame =
    val oldNext = next
    nextVar = last
    if oldNext != null then
      oldNext.reverseInPlace(this)
    else
      this


  override def toString: String =
    val b = if isBase then "" else "%"
    val s = s"+${delta}${b}${step}"
    if next == null then
      s
    else
      s"$s;${next.toString}"


private object Frame:
  val base: Frame = pushFirst(Step.Pop, isNested = false, FrameKind.plain)

  def pushFirst(step: Step, isNested: Boolean, kind: FrameKind): Frame =
    new Frame(
      nextVar = null,
      packedVar = FramePacked(0, isNested, kind),
      step = step,
      local = Local.nul,
    )


  final case class Split(
    frameHi: Frame,
    frameLo: Frame,
    heightHi: Int,
    heightLo: Int,
    local: Local,
  )

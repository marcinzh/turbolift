package turbolift.internals.engine.stacked
import Pile.{Split1, Split2}


//@#@TODO reconsider misleading naming convention
//// Caution: any `height` variable (`maxHeight`, `divHeight`, etc.)
//// is actually a 0-based index (into `Stack.piles` array)
private final class Pile private (
  val prompt: Prompt,
  val topFrame: Frame,
  val minHeight: Int,
  val maxHeight: Int,
  val hasBase: Boolean,
):
  assert(minHeight <= maxHeight)
  assert(topFrame.bottom.isBase == hasBase)
  assert(topFrame.computeBottomHeight(maxHeight) == minHeight)


  private def copy(
    prompt: Prompt = prompt,
    topFrame: Frame = topFrame,
    minHeight: Int = minHeight,
    maxHeight: Int = maxHeight,
    hasBase: Boolean = hasBase,
  ): Pile = new Pile(
    prompt = prompt,
    topFrame = topFrame,
    minHeight = minHeight,
    maxHeight = maxHeight,
    hasBase = hasBase,
  )


  def pushNested(step: Step, local: Local, height: Int, kind: FrameKind): Pile =
    val newFrame = topFrame.pushNext(step, local, height - maxHeight, isNested = true, kind)
    copy(
      topFrame = newFrame,
      maxHeight = height,
    )


  def pop: Pile =
    copy(
      topFrame = topFrame.next.nn,
      maxHeight = maxHeight - topFrame.delta,
    )


  def split(divHeight: Int, oldLocal: Local, truncate: Boolean): Split2 =
    if divHeight < minHeight then
      //// Fast path: all frames are ABOVE div
      val hi =
        val newPile = copy(
          minHeight = minHeight - divHeight,
          maxHeight = maxHeight - divHeight,
        )
        Split1(newPile, oldLocal)
      (hi, null)
    else if maxHeight < divHeight then
      //// Fast path: all frames are BELOW div
      val lo = Split1(this, oldLocal)
      (null, lo)
    else 
      //// div is in range `minHeight`..`maxHeight`
      if maxHeight == divHeight then
        //// Special case: the split happens at this pile's prompt
        val lo =
          if minHeight == maxHeight then
            //// This pile has single frame. Discard it
            assert(!topFrame.hasNext)
            null
          else
            //// Like `drop(1)` applied to list of frames
            val newPile = copy(
              topFrame = topFrame.next.nn,
              maxHeight = maxHeight - topFrame.delta,
            )
            Split1(newPile, topFrame.local)
        val hi =
          if truncate then
            null
          else
            //// Like `take(1)` applied to list of frames
            //@#@OPTY reuse
            val newFrame = topFrame.dropNext
            val newPile = copy(
              minHeight = 0,
              maxHeight = 0,
              topFrame = newFrame,
              hasBase = newFrame.isBase,
            )
            Split1(newPile, oldLocal)
        (hi, lo)
      else
        //// General case: split tha list of frames somewhere in the middle
        val split = topFrame.split(initialHeight = maxHeight, divHeight = divHeight)
        val hi =
          val newPile = copy(
            topFrame = split.frameHi,
            minHeight = split.heightHi - divHeight,
            maxHeight = maxHeight - divHeight,
            hasBase = false,
          )
          Split1(newPile, oldLocal)
        val lo =
          val newPile = copy(
            topFrame = split.frameLo,
            maxHeight = split.heightLo,
          )
          Split1(newPile, split.local)
        (hi, lo)


  override def toString =
    val a = if hasBase then "" else "%"
    val b = topFrame.toString
    val c = if maxHeight == minHeight then s"$maxHeight" else s"$maxHeight-$minHeight"
    s"[$c]$b$a"


private object Pile:
  def initial(prompt: Prompt): Pile = base(prompt, 1)

  def base(prompt: Prompt, i: Int): Pile = 
    new Pile(
      prompt = prompt,
      topFrame = Frame.base,
      minHeight = i,
      maxHeight = i,
      hasBase = true,
    )

  def pushFirst(prompt: Prompt, step: Step, height: Int, isNested: Boolean, kind: FrameKind): Pile =
    new Pile(
      prompt = prompt,
      topFrame = Frame.pushFirst(step, isNested, kind),
      minHeight = height,
      maxHeight = height,
      hasBase = !isNested,
    )


  final case class Split1(pile: Pile, local: Local):
    def ord: Int = pile.minHeight

  type Split2 = (Split1 | Null, Split1 | Null)

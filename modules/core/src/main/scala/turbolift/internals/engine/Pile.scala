package turbolift.internals.engine


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

  //------------------------------------------------------------------------------
  //@#@TODO separate `splitHi` & `splitLo` no longer needed. Make single `split`
  //------------------------------------------------------------------------------

  //// `splitHi` doesn't use `Local` param. It's here only to make `splitHi` have the same type as `splitLo`
  def splitHi(divHeight: Int, oldLocal: Local): (Pile | Null, Local) =
    (splitHiForReal(divHeight), oldLocal)


  private def splitHiForReal(divHeight: Int): Pile | Null =
    if divHeight < minHeight then
      //// all frames are ABOVE div ==> result := full
      copy(
        minHeight = minHeight - divHeight,
        maxHeight = maxHeight - divHeight,
      )
    else if maxHeight < divHeight then
      //// all frames are BELOW div ==> result := empty
      null
    else if maxHeight == divHeight then
      //// exactly AT div ==> result := single frame
      copy(
        topFrame = topFrame.bridge,
        minHeight = 0,
        maxHeight = 0,
        hasBase = topFrame.isBase,
      )
    else
      //// div is in the middle ==> drop suffix
      val (newFrame, newHeight, newHasBase) = topFrame.splitHi(initialHeight = maxHeight, divHeight = divHeight)
      copy(
        topFrame = newFrame,
        minHeight = newHeight - divHeight,
        maxHeight = maxHeight - divHeight,
        hasBase = newHasBase,
      )


  def splitLo(divHeight: Int, oldLocal: Local): (Pile | Null, Local) =
    if divHeight <= minHeight then
      //// all frames are AT or ABOVE div ==> result := empty
      (null, oldLocal)
    else if maxHeight < divHeight then
      //// all frames are BELOW div ==> result := full
      (this, oldLocal)
    else
      //// div is in the middle ==> drop prefix
      val (newFrame, newHeight, newLocal) = topFrame.splitLo(initialHeight = maxHeight, divHeight = divHeight, oldLocal)
      val newPile = copy(
        topFrame = newFrame,
        maxHeight = newHeight,
      )
      (newPile, newLocal)


  override def toString =
    val a = if hasBase then "" else "%"
    val b = topFrame.toString
    s"$b$a"


private object Pile:
  def pushFirst(prompt: Prompt, step: Step, height: Int, isNested: Boolean, kind: FrameKind): Pile =
    new Pile(
      prompt = prompt,
      topFrame = Frame.pushFirst(step, isNested, kind),
      minHeight = height,
      maxHeight = height,
      hasBase = !isNested,
    )

  def initial(prompt: Prompt): Pile = base(prompt, 1)

  def base(prompt: Prompt, i: Int): Pile = 
    new Pile(
      prompt = prompt,
      topFrame = Frame.base,
      minHeight = i,
      maxHeight = i,
      hasBase = true,
    )

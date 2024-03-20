package turbolift.internals.engine


private[internals] final class Pile private (
  val topFrame: Frame,
  val minHeight: Int,
  val maxHeight: Int,
  val hasBase: Boolean,
):
  assert(minHeight <= maxHeight)
  assert(topFrame.bottom.isBase == hasBase)
  assert(topFrame.computeBottomHeight(maxHeight) == minHeight)


  private def copy(
    topFrame: Frame = topFrame,
    minHeight: Int = minHeight,
    maxHeight: Int = maxHeight,
    hasBase: Boolean = hasBase,
  ): Pile = new Pile(
    topFrame = topFrame,
    minHeight = minHeight,
    maxHeight = maxHeight,
    hasBase = hasBase,
  )


  def pushLocal(step: Step, stan: Stan, height: Int, kind: FrameKind): Pile =
    val newFrame = topFrame.pushNext(step, stan, height - maxHeight, isLocal = true, kind)
    copy(
      topFrame = newFrame,
      maxHeight = height,
    )


  def pop: Pile =
    copy(
      topFrame = topFrame.next.nn,
      maxHeight = maxHeight - topFrame.delta,
    )

  //// `splitHi` doesn't use `Stan` param. It's here only to make `splitHi` have the same type as `splitLo`
  def splitHi(divHeight: Int, oldStan: Stan): (Pile | Null, Stan) =
    (splitHiForReal(divHeight), oldStan)


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
      new Pile(
        topFrame = topFrame.bridge,
        minHeight = 0,
        maxHeight = 0,
        hasBase = topFrame.isBase,
      )
    else
      //// div is in the middle ==> drop suffix
      val (newFrame, newHeight, newHasBase) = topFrame.splitHi(initialHeight = maxHeight, divHeight = divHeight)
      new Pile(
        topFrame = newFrame,
        minHeight = newHeight - divHeight,
        maxHeight = maxHeight - divHeight,
        hasBase = newHasBase,
      )


  def splitLo(divHeight: Int, oldStan: Stan): (Pile | Null, Stan) =
    if divHeight <= minHeight then
      //// all frames are AT or ABOVE div ==> result := empty
      (null, oldStan)
    else if maxHeight < divHeight then
      //// all frames are BELOW div ==> result := full
      (this, oldStan)
    else
      //// div is in the middle ==> drop prefix
      val (newFrame, newHeight, newStan) = topFrame.splitLo(initialHeight = maxHeight, divHeight = divHeight, oldStan)
      val newPile = copy(
        topFrame = newFrame,
        maxHeight = newHeight,
      )
      (newPile, newStan)


  override def toString =
    val a = if hasBase then "" else "%"
    val b = topFrame.toString
    s"$b$a"


private[engine] object Pile:
  def pushFirst(step: Step, height: Int, isLocal: Boolean, kind: FrameKind): Pile =
    new Pile(
      topFrame = Frame.pushFirst(step, isLocal, kind),
      minHeight = height,
      maxHeight = height,
      hasBase = !isLocal,
    )


  def base(i: Int): Pile = 
    if i < STOCK_SIZE then
      stock(i)
    else
      makeBase(i)

  inline private val STOCK_SIZE = 20
  private val stock: Array[Pile] = Array.tabulate(STOCK_SIZE)(makeBase)

  private def makeBase(i: Int): Pile = 
    new Pile(
      topFrame = Frame.base,
      minHeight = i,
      maxHeight = i,
      hasBase = true,
    )

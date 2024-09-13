package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.Signature
import turbolift.interpreter.{Features, Interpreter}


private[engine] final class Stack private (
  //@#@OPTY use var for append-in-place for one-shot resump
  val lookup: Lookup,
  val piles: Array[Pile],
  val frameCount: Int,
  val headFeatures: Features,
  var fork: Stack,
  var tailOrJoin: Stack | Null,
  var aside: Step | Null,
):
  private def this(
    lookup: Lookup,
    piles: Array[Pile],
    frameCount: Int,
    headFeatures: Features,
  ) = this(
    lookup = lookup,
    piles = piles,
    frameCount = frameCount,
    headFeatures = headFeatures,
    fork = null.asInstanceOf[Stack],
    tailOrJoin = null,
    aside = null,
  )

  private def this(
    sigCount: Int,
    promptCount: Int,
    frameCount: Int,
    headFeatures: Features,
  ) = this(
    lookup = Lookup.blank(sigCount),
    piles = new Array[Pile](promptCount),
    frameCount = frameCount,
    headFeatures = headFeatures,
  )

  val accumFeatures: Features = if isTailless then headFeatures else headFeatures | tail.accumFeatures

  def isTailless: Boolean = aside == null
  def tail: Stack = { assert(!isTailless); tailOrJoin.nn }
  def canPop: Boolean = if isTailless then frameCount > 1 else true
  def promptCount: Int = piles.size
  def promptIter: Iterator[Prompt] = piles.iterator.map(_.prompt)


  //-------------------------------------------------------------------
  // Lookup
  //-------------------------------------------------------------------


  @tailrec def containsSignature(sig: Signature): Boolean =
    if lookup.containsSignature(sig) then
      true
    else
      if isTailless then
        false
      else
        tail.containsSignature(sig)


  @tailrec def findEntryBySignature(sig: Signature, depth: Int = 0): Entry =
    val entry = lookup.findBySignature(sig)
    if entry != null then
      entry.copyWithDepth(depth)
    else
      if isTailless then
        Lookup.sigNotFound(sig)
      else
        tail.findEntryBySignature(sig, depth + 1)

  
  def findEntryByPrompt(prompt: Prompt): Entry = findEntryBySignature(prompt.signatures.head)


  //@#@TODO update callers
  //// {{ old interface
  def locateIO: Location.Deep = locatePrompt(Prompt.IO)
  def locatePrompt(prompt: Prompt): Location.Deep = findEntryByPrompt(prompt).location
  //// }} old interface


  def locateHighestPile: Location.Shallow =
    //@#@OPTY search in reverse order
    val n = frameCount - 1
    @tailrec def loop(i: Int, j: Int): Location.Shallow =
      val pile = piles(i)
      if pile.maxHeight == n then
        Location.Shallow(promptIndex = i, storeIndex = j)
      else
        loop(i + 1, j + pile.prompt.localCount)
    loop(0, 0)


  //-------------------------------------------------------------------
  // Push & Pop
  //-------------------------------------------------------------------


  def pushNewSegment(aside: Step, prompt: Prompt, isNested: Boolean, kind: FrameKind): Stack =
    Stack.newSegment(prompt, isNested, kind, this, aside)


  def pushBase(prompt: Prompt, step: Step, localIndex: Int): Stack =
    val features = prompt.features.mask
    val newPileFork = Pile.base(prompt, promptCount)
    val newPileMain = Pile.pushFirst(prompt, step, height = frameCount, isNested = false, FrameKind.plain)
    val newFork = fork.pushBase_Aux(newPileFork, prompt, features, localIndex)
    val newMain = this.pushBase_Aux(newPileMain, prompt, features, localIndex)
    newMain.fixForkSegment(newFork)


  private def pushBase_Aux(pile: Pile, prompt: Prompt, features: Features, localIndex: Int): Stack =
    val newEntry = Entry(prompt, Location.Shallow(promptIndex = promptCount, storeIndex = localIndex))
    new Stack(
      lookup = lookup.push(newEntry),
      piles = piles :+ pile,
      frameCount = frameCount + 1,
      headFeatures = headFeatures | features,
      fork = null.asInstanceOf[Stack],
      tailOrJoin = tailOrJoin,
      aside = aside,
    )


  def popLast(prompt: Prompt): Stack =
    //@#@OPTY share components when possible
    if piles.last.hasBase then
      val newFork = fork.popLast_Aux(prompt)
      val newMain = this.popLast_Aux(prompt)
      newMain.fixForkSegment(newFork)
    else
      popLast_Aux(prompt)


  private def popLast_Aux(prompt: Prompt): Stack =
    val newPiles = piles.init
    new Stack(
      lookup = lookup.pop,
      piles = newPiles,
      frameCount = frameCount - 1,
      headFeatures = newPiles.iterator.map(_.prompt.features).reduce(_ | _), //@#@OPTY
      fork = fork,
      tailOrJoin = tailOrJoin,
      aside = aside,
    )


  def pushNested(promptIndex: Int, step: Step, savedLocal: Local, kind: FrameKind): Stack =
    val oldPile = piles(promptIndex)
    val newPile = oldPile.pushNested(step, savedLocal, height = frameCount, kind)
    pushPopNested_Aux(newPile, promptIndex, +1)


  def popNested(promptIndex: Int): Stack =
    val oldPile = piles(promptIndex)
    val newPile = oldPile.pop
    pushPopNested_Aux(newPile, promptIndex, -1)

    
  private def pushPopNested_Aux(newPile: Pile, promptIndex: Int, frameCountDiff: Int): Stack =
    new Stack(
      lookup = lookup,
      piles = piles.updated(promptIndex, newPile),
      frameCount = frameCount + frameCountDiff,
      headFeatures = headFeatures,
      fork = fork,
      tailOrJoin = tailOrJoin,
      aside = aside,
    )


  //-------------------------------------------------------------------
  // Fork & Join
  //-------------------------------------------------------------------


  @tailrec def lastSegment: Stack = if isTailless then this else tail.lastSegment

  def getJoin: Stack = lastSegment.tailOrJoin.nn


  private def fixForkSegment(): Stack =
    fork = this
    tailOrJoin = this
    this


  private def fixForkSegment(that: Stack): Stack =
    that.fixForkSegment()
    fork = that
    this


  def makeFork: Stack =
    if isTailless then
      fork
    else
      val that = makeForkDeep
      lastSegment.tailOrJoin = that
      that


  private def makeForkDeep: Stack =
    if isTailless then
      fork
    else
      fork.copyWithTail(tail = tail.makeForkDeep, aside = StepCases.Pop)


  def hasSamePromptsAs(that: Stack): Boolean =
    if this eq that then
      true
    else
      promptIter.sameElements(that.promptIter) && {
        if isTailless && that.isTailless then
          true
        else
          if !isTailless && !that.isTailless then
            tail.hasSamePromptsAs(that.tail)
          else
            false
      }


  //-------------------------------------------------------------------
  // Misc
  //-------------------------------------------------------------------


  def setTailInPlace(tail: Stack | Null, aside: Step | Null): Unit =
    this.tailOrJoin = tail
    this.aside = aside


  def copyWithTail(tail: Stack | Null, aside: Step | Null): Stack =
    new Stack(
      lookup = lookup,
      piles = piles,
      frameCount = frameCount,
      headFeatures = headFeatures,
      fork = fork,
      tailOrJoin = tail,
      aside = aside,
    )


  def ::?(that: (Stack | Null, Step | Null)): Stack = copyWithTail(tail = that._1, aside = that._2)

  override def toString: String = toStr
  final def toStr: String = s"Stack(${toStrAux})"

  final def toStrAux: String =
    val a =
      val aa =
        (for i <- 0.until(promptCount) yield
          s"${piles(i).prompt}:${piles(i)}"
        ).reverse.mkString(", ")
      s"[$aa]"

    if isTailless then
      a
    else
      val b = aside.toString
      val c = tail.toStrAux
      s"$a |$b| $c"


private[engine] object Stack:
  val initial: Stack =
    newSegment(
      prompt = Prompt.IO,
      isNested = false,
      kind = FrameKind.plain,
      tail = null.asInstanceOf[Stack],
      aside = null,
    )


  private val emptyForkSegment: Stack =
    newForkSegment(
      lookup = Lookup.empty,
      piles = Array(),
      frameCount = 0,
      headFeatures = Features.Empty,
    )


  def newSegment(
    prompt: Prompt,
    isNested: Boolean,
    kind: FrameKind,
    tail: Stack,
    aside: Step | Null,
  ): Stack =
    val lookup = Lookup.empty.push(Entry(prompt, Location.Shallow(0, 0)))
    val piles = Array(Pile.pushFirst(prompt, StepCases.Pop, 0, isNested, kind))
    val headFeatures = prompt.features.mask
    val fork =
      if isNested then
        emptyForkSegment
      else
        newForkSegment(
          lookup = lookup,
          piles = piles,
          frameCount = 1,
          headFeatures = headFeatures,
        )
    new Stack(
      lookup = lookup,
      piles = piles,
      frameCount = 1,
      headFeatures = headFeatures,
      tailOrJoin = tail,
      aside = aside,
      fork = fork,
    )


  private def newForkSegment(
    lookup: Lookup,
    piles: Array[Pile],
    frameCount: Int,
    headFeatures: Features,
  ): Stack =
    new Stack(
      lookup = lookup,
      piles = piles,
      frameCount = frameCount,
      headFeatures = headFeatures,
    )
    .fixForkSegment()


  def blank(
    sigCount: Int,
    promptCount: Int,
    frameCount: Int,
    headFeatures: Features,
  )(
    forkSigCount: Int,
    forkPromptCount: Int,
    forkHeadFeatures: Features,
  ): Stack =
    val fork = new Stack(
      sigCount = forkSigCount,
      promptCount = forkPromptCount,
      frameCount = forkPromptCount, //// 1 frame in each pile
      headFeatures = forkHeadFeatures,
    )
    val main = new Stack(
      sigCount = sigCount,
      promptCount = promptCount,
      frameCount = frameCount,
      headFeatures = headFeatures,
    )
    main.fixForkSegment(fork)

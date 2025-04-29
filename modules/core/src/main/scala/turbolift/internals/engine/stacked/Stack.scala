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
  private var tailVar: Stack | Null,
  private var asideVar: Step | Null,
) extends Cloneable:
  private var forkVar: Stack | Null = null
  val accumFeatures: Features = if isTailless then headFeatures else headFeatures | tail.accumFeatures

  def isTailless: Boolean = !hasTail
  def hasTail: Boolean = tailVar != null
  def tail: Stack = tailVar.nn
  def tailOrNull: Stack | Null = tailVar
  def aside: Step = asideVar.nn
  def asideOrNull: Step | Null = asideVar
  def canPop: Boolean = hasTail || frameCount > 1
  def isEmptySegment: Boolean = piles.isEmpty
  def nonEmptySegment: Boolean = piles.nonEmpty
  def promptCount: Int = piles.size
  def promptIter: Iterator[Prompt] = piles.iterator.map(_.prompt)


  //-------------------------------------------------------------------
  // Lookup
  //-------------------------------------------------------------------


  @tailrec def containsSignature(sig: Signature): Boolean =
    if lookup.containsSignature(sig) then
      true
    else
      if !isTailless then
        tail.containsSignature(sig)
      else
        false


  @tailrec def findEntryBySignature(sig: Signature, depth: Int = 0): Entry =
    val entry = lookup.findBySignature(sig)
    if entry != null then
      entry.copyWithDepth(depth)
    else
      if !isTailless then
        tail.findEntryBySignature(sig, depth + 1)
      else
        Stack.sigNotFound(sig)


  @tailrec def findEntryByPrompt(prompt: Prompt, depth: Int = 0): Entry =
    val entry = lookup.findByPrompt(prompt)
    if entry != null then
      entry.copyWithDepth(depth)
    else
      if !isTailless then
        tail.findEntryByPrompt(prompt, depth + 1)
      else
        Stack.promptNotFound(prompt)


  @tailrec def findEntryBySignatureWithShadow(sig: Signature, shadowCount: Int, depth: Int = 0): Entry =
    lookup.findBySignatureWithShadow(sig, shadowCount) match
      case entry: Entry => entry.copyWithDepth(depth)
      case remainingShadow: Int =>
        if isTailless then
          Stack.sigNotFound(sig)
        else
          tail.findEntryBySignatureWithShadow(sig, shadowCount = remainingShadow, depth + 1)


  @tailrec def findEntryByPromptWithShadow(prompt: Prompt, shadowCount: Int, depth: Int = 0): Entry =
    lookup.findByPromptWithShadow(prompt, shadowCount) match
      case entry: Entry => entry.copyWithDepth(depth)
      case remainingShadow: Int =>
        if isTailless then
          Stack.promptNotFound(prompt)
        else
          tail.findEntryByPromptWithShadow(prompt, shadowCount = remainingShadow, depth + 1)


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
    Stack.newSegment(
      prompt = prompt,
      isNested = isNested,
      kind = kind,
      tail = this,
      aside = aside,
    )


  def pushBase(prompt: Prompt, step: Step, localIndex: Int): Stack =
    val newPiles =
      val newPile = Pile.pushFirst(prompt, step, height = frameCount, isNested = false, FrameKind.plain)
      piles :+ newPile
    val newLookup =
      val newLocation = Location.Shallow(promptIndex = promptCount, storeIndex = localIndex)
      val newEntry = Entry(prompt, newLocation)
      lookup.push(newEntry)
    new Stack(
      lookup = newLookup,
      piles = newPiles,
      frameCount = frameCount + 1,
      headFeatures = headFeatures | prompt.features.mask,
      tailVar = tailVar,
      asideVar = asideVar,
    )


  def popLast(prompt: Prompt): Stack =
    val newPiles = piles.init
    new Stack(
      lookup = lookup.pop,
      piles = newPiles,
      frameCount = frameCount - 1,
      headFeatures = newPiles.iterator.map(_.prompt.features).reduce(_ | _), //@#@OPTY
      tailVar = tailVar,
      asideVar = asideVar,
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
    val that = new Stack(
      lookup = lookup,
      piles = piles.updated(promptIndex, newPile),
      frameCount = frameCount + frameCountDiff,
      headFeatures = headFeatures,
      tailVar = tailVar,
      asideVar = asideVar,
    )
    that.forkVar = forkVar
    that


  //-------------------------------------------------------------------
  // Fork
  //-------------------------------------------------------------------


  def lazyFork: Stack =
    //// harmless race
    if forkVar == null then
      forkVar = makeFork
    forkVar.nn


  private def makeFork: Stack =
    var forkPromptCount = 0
    var forkSigCount = 0
    var forkLocalCount = 0
    var forkFeatures = Features.Empty
    {
      val n = piles.size
      var i = 0
      while i < n do
        val pile = piles(i)
        if pile.hasBase then
          val prompt = pile.prompt
          forkPromptCount += 1
          forkSigCount += prompt.signatures.size
          forkLocalCount += prompt.localCount
          forkFeatures |= prompt.features.mask
        i += 1
    }
    if forkPromptCount == 0 then
      tail.lazyFork
    else
      val forkPiles = new Array[Pile](forkPromptCount)
      val forkLookup = Lookup.blank(forkSigCount)
      {
        var forkPromptIndex = 0
        var forkLookupIndex = forkLookup.initialIndexForSetInPlace
        var forkLocalIndex = 0
        val n = piles.size
        var i = 0
        while i < n do
          val pile = piles(i)
          if pile.hasBase then
            val prompt = pile.prompt
            val forkPile = Pile.base(prompt, forkPromptIndex)
            val entry = new Entry(pile.prompt, promptIndex = forkPromptIndex, storeIndex = forkLocalIndex)
            forkPiles(forkPromptIndex) = forkPile
            forkLookupIndex = forkLookup.setInPlace(forkLookupIndex, entry)
            forkPromptIndex += 1
            forkLocalIndex += prompt.localCount
          i += 1
      }
      val forkStack = new Stack(
        lookup = forkLookup,
        piles = forkPiles,
        frameCount = forkPromptCount,
        headFeatures = forkFeatures,
        tailVar = if hasTail then tail.lazyFork else null,
        asideVar = Step.Pop,
      )
      forkStack.forkVar = forkStack
      forkStack


  //// used by ZipperImpl
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


  def localCount: Int =
    var s = 0
    val n = piles.size
    var i = 0
    while i < n do
      s += piles(i).prompt.localCount
      i += 1
    s


  //-------------------------------------------------------------------
  // Misc
  //-------------------------------------------------------------------


  def setTailInPlace(tail: Stack | Null): Unit =
    this.tailVar = tail


  def setTailInPlace2(tail: Stack | Null, aside: Step | Null): Unit =
    this.tailVar = tail
    this.asideVar = aside


  def copyWithTail(tail: Stack | Null): Stack =
    val that = clone().asInstanceOf[Stack]
    that.tailVar = tail
    that

  def copyWithTail2(tail: Stack | Null, aside: Step | Null): Stack =
    val that = clone().asInstanceOf[Stack]
    that.tailVar = tail
    that.asideVar = aside
    that


  override def toString: String = toStr
  def toStr: String = s"Stack(${toStrAux})"

  def toStrAux: String =
    val a =
      val aa =
        (for i <- 0.until(promptCount) yield
          s"${piles(i).prompt}:${piles(i)}"
        ).reverse.mkString(", ")
      s"[$aa]"

    if isTailless then
      a
    else
      val b = asideOrNull
      val c = tail.toStrAux
      s"$a |$b| $c"


  def integrityCheck(): Unit =
    val arr = new Array[Int](frameCount)
    for pile <- piles do
      def loop(frame: Frame, currHeight: Int): Unit =
        arr(currHeight) += 1
        if frame.hasNext then
          loop(frame.next.nn, currHeight - frame.delta)
        else
          assert(currHeight == pile.minHeight)
      loop(pile.topFrame, pile.maxHeight)
    assert(arr.forall(_ == 1))


private[engine] object Stack:
  val initial: Stack =
    val stack = newSegment(
      prompt = Prompt.IO,
      isNested = false,
      kind = FrameKind.plain,
      tail = null.asInstanceOf[Stack],
      aside = null,
    )
    stack.forkVar = stack
    stack


  val empty: Stack =
    new Stack(
      lookup = Lookup.empty,
      piles = Array.empty[Pile],
      frameCount = 0,
      headFeatures = Features.Empty,
      tailVar = null,
      asideVar = null,
    )


  def newSegment(
    prompt: Prompt,
    isNested: Boolean,
    kind: FrameKind,
    tail: Stack,
    aside: Step | Null,
  ): Stack =
    val lookup = Lookup.empty.push(Entry(prompt, 0, 0))
    val piles = Array(Pile.pushFirst(prompt, Step.Pop, 0, isNested, kind))
    val headFeatures = prompt.features.mask
    new Stack(
      lookup = lookup,
      piles = piles,
      frameCount = 1,
      headFeatures = headFeatures,
      tailVar = tail,
      asideVar = aside,
    )


  def blank(
    sigCount: Int,
    promptCount: Int,
    frameCount: Int,
    headFeatures: Features,
  ): Stack =
    new Stack(
      lookup = Lookup.blank(sigCount),
      piles = new Array[Pile](promptCount),
      frameCount = frameCount,
      headFeatures = headFeatures,
      asideVar = null,
      tailVar = null,
    )


  def sigNotFound(s: Signature): Nothing = panic(s"Signature ${s} not found")
  def promptNotFound(p: Prompt): Nothing = panic(s"Prompt ${p} not found")

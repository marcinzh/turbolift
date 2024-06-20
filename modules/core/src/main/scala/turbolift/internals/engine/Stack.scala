package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.Signature
import turbolift.interpreter.{Features, Interpreter}




private final class Stack private (
  //@#@OPTY use var for append-in-place for one-shot resump
  val signatures: Array[Signature],
  val locations: Array[Location.Shallow],
  val prompts: Array[Prompt],
  val piles: Array[Pile],
  val frameCount: Int,
  val headFeatures: Features,
  var fork: Stack,
  var tailOrJoin: Stack | Null,
  var aside: Step | Null,
):
  private def this(
    signatures: Array[Signature],
    locations: Array[Location.Shallow],
    prompts: Array[Prompt],
    piles: Array[Pile],
    frameCount: Int,
    headFeatures: Features,
  ) = this(
    signatures = signatures,
    locations = locations,
    prompts = prompts,
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
    signatures = new Array(sigCount),
    locations = new Array(sigCount),
    prompts = new Array(promptCount),
    piles = new Array(promptCount),
    frameCount = frameCount,
    headFeatures = headFeatures,
  )

  val accumFeatures: Features = if isTailless then headFeatures else headFeatures | tail.accumFeatures

  def isTailless: Boolean = aside == null
  def tail: Stack = { assert(!isTailless); tailOrJoin.nn }
  def canPop: Boolean = if isTailless then frameCount > 1 else true
  def nextPromptIndex: Int = piles.size


  //-------------------------------------------------------------------
  // Lookup
  //-------------------------------------------------------------------


  @tailrec def containsSignature(sig: Signature): Boolean =
    val i = signatures.indexOf(sig)
    if i < 0 then
      if isTailless then
        false
      else
        tail.containsSignature(sig)
    else
      true


  @tailrec def locateSignature(sig: Signature, cache: Cache, depth: Int = 0): Unit =
    val i = signatures.lastIndexOf(sig)
    if i >= 0 then
      val location = locations(i)
      val prompt = prompts(location.promptIndex)
      cache.prompt = prompt
      cache.location = location.withDepth(depth)
    else
      if isTailless then
        sigNotFound(sig)
      else
        tail.locateSignature(sig, cache, depth + 1)


  def locatePrompt(interp: Interpreter.Untyped, cache: Cache): Unit =
    locateSignature(interp.signatures.head, cache)


  def locateIO: Location.Deep =
    val sig = PromptIO.signatures.head
    @tailrec def loop(todo: Stack, depth: Int): Location.Deep =
      val i = signatures.lastIndexOf(sig)
      if i >= 0 then
        locations(i).withDepth(depth)
      else
        if isTailless then
          sigNotFound(sig)
        else
          loop(tail, depth + 1)
    loop(this, 0)


  private def sigNotFound(s: Signature): Nothing = panic(s"Signature ${s} not found")


  def locateHighestPile: Location.Shallow =
    //@#@OPTY search in reverse order
    val n = frameCount - 1
    @tailrec def loop(i: Int, j: Int): Location.Shallow =
      val pile = piles(i)
      val prompt = prompts(i)
      if pile.maxHeight == n then
        Location.Shallow(promptIndex = i, localIndex = j, isStateful = prompt.isStateful)
      else
        loop(i + 1, j + prompt.localCount)
    loop(0, 0)


  //-------------------------------------------------------------------
  // Push & Pop
  //-------------------------------------------------------------------


  def pushNewSegment(aside: Step, prompt: Prompt, isNested: Boolean, kind: FrameKind): Stack =
    Stack.newSegment(prompt, isNested, kind, this, aside)


  def pushBase(loc: Location.Shallow, step: Step, prompt: Prompt): Stack =
    //// shared {{
    val newSignatures = signatures ++ prompt.signatures
    val newLocations = locations ++ Array.fill(prompt.signatures.size)(loc)
    val newPrompts = prompts :+ prompt
    val newFeatures = prompt.features.mask
    //// }}
    val newPileFork = Pile.base(loc.promptIndex)
    val newPileMain = Pile.pushFirst(step, height = frameCount, isNested = false, FrameKind.plain)
    val newFork = fork.pushBase_Aux(newPileFork, newSignatures, newLocations, newPrompts, newFeatures)
    val newMain = this.pushBase_Aux(newPileMain, newSignatures, newLocations, newPrompts, newFeatures)
    newMain.fixForkSegment(newFork)


  private def pushBase_Aux(
    newPile: Pile,
    newSignatures: Array[Signature],
    newLocations: Array[Location.Shallow],
    newPrompts: Array[Prompt],
    newFeatures: Features,
  ): Stack =
    new Stack(
      signatures = newSignatures,
      locations = newLocations,
      prompts = newPrompts,
      piles = piles :+ newPile,
      frameCount = frameCount + 1,
      headFeatures = headFeatures | newFeatures,
      fork = null.asInstanceOf[Stack],
      tailOrJoin = tailOrJoin,
      aside = aside,
    )


  def popLast(prompt: Prompt): Stack =
    assert(prompt == prompts.last)
    //@#@OPTY share components when possible
    if piles.last.hasBase then
      val newFork = fork.popLast_Aux(prompt)
      val newMain = this.popLast_Aux(prompt)
      newMain.fixForkSegment(newFork)
    else
      popLast_Aux(prompt)


  private def popLast_Aux(prompt: Prompt): Stack =
    val newPrompts = prompts.init
    val newSigCount = signatures.size - prompt.signatures.size
    new Stack(
      signatures = signatures.take(newSigCount),
      locations = locations.take(newSigCount),
      prompts = newPrompts,
      piles = piles.init,
      frameCount = frameCount - 1,
      headFeatures = newPrompts.iterator.map(_.features).reduce(_ | _), //@#@OPTY
      fork = fork,
      tailOrJoin = tailOrJoin,
      aside = aside,
    )


  def pushNested(loc: Location.Shallow, step: Step, savedLocal: Local, kind: FrameKind): Stack =
    val oldPile = piles(loc.promptIndex)
    val newPile = oldPile.pushNested(step, savedLocal, height = frameCount, kind)
    pushPopNested_Aux(newPile, loc.promptIndex, +1)


  def popNested(loc: Location.Shallow): Stack =
    val oldPile = piles(loc.promptIndex)
    val newPile = oldPile.pop
    pushPopNested_Aux(newPile, loc.promptIndex, -1)

    
  private def pushPopNested_Aux(newPile: Pile, promptIndex: Int, frameCountDiff: Int): Stack =
    new Stack(
      signatures = signatures,
      locations = locations,
      prompts = prompts,
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
      prompts.sameElements(that.prompts) && {
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
      signatures = signatures,
      locations = locations,
      prompts = prompts,
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
        (for i <- 0.until(prompts.size) yield
          s"${prompts(i)}:${piles(i)}"
        ).reverse.mkString(", ")
      s"[$aa]"

    if isTailless then
      a
    else
      val b = aside.toString
      val c = tail.toStrAux
      s"$a |$b| $c"


//-------------------------------------------------------------------


private object Stack:
  val initial: Stack =
    newSegment(
      prompt = PromptIO,
      isNested = false,
      kind = FrameKind.plain,
      tail = null.asInstanceOf[Stack],
      aside = null,
    )


  private val emptyForkSegment: Stack =
    newForkSegment(
      signatures = Array(),
      locations = Array(),
      prompts = Array(),
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
    val signatures = prompt.signatures
    val locations = Array.fill(signatures.size)(Location.Shallow(0, 0, prompt.isStateful))
    val prompts = Array(prompt)
    val piles = Array(Pile.pushFirst(StepCases.Pop, 0, isNested, kind))
    val frameCount = 1
    val headFeatures = prompt.features.mask
    val fork =
      if isNested then
        emptyForkSegment
      else
        newForkSegment(
          signatures = signatures,
          locations = locations,
          prompts = prompts,
          piles = piles,
          frameCount = frameCount,
          headFeatures = headFeatures,
        )
    new Stack(
      signatures = signatures,
      locations = locations,
      prompts = prompts,
      piles = piles,
      frameCount = frameCount,
      headFeatures = headFeatures,
      tailOrJoin = tail,
      aside = aside,
      fork = fork,
    )


  private def newForkSegment(
    signatures: Array[Signature],
    locations: Array[Location.Shallow],
    prompts: Array[Prompt],
    piles: Array[Pile],
    frameCount: Int,
    headFeatures: Features,
  ): Stack =
    new Stack(
      signatures = signatures,
      locations = locations,
      prompts = prompts,
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

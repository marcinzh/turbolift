package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.Signature
import turbolift.interpreter.{Features, Interpreter}


private final class Stack private (
  //@#@OPTY use var for append-in-place for one-shot resump
  var tailOrNull: Stack | Null,
  var aside: Step | Null,
  val signatures: Array[Signature],
  val locations: Array[Location.Shallow],
  val prompts: Array[Prompt],
  val piles: Array[Pile],
  val frameCount: Int,
  headFeatures: Features,
  forkOrNull: Stack | Null,
):
  private def copy(
    tailOrNull: Stack | Null = tailOrNull,
    aside: Step | Null = aside,
    signatures: Array[Signature] = signatures,
    locations: Array[Location.Shallow] = locations,
    prompts: Array[Prompt] = prompts,
    piles: Array[Pile] = piles,
    frameCount: Int = frameCount,
    headFeatures: Features = headFeatures,
    forkOrNull: Stack | Null = fork,
  ): Stack =
    new Stack(
      tailOrNull = tailOrNull,
      aside = aside,
      signatures = signatures,
      locations = locations,
      prompts = prompts,
      piles = piles,
      frameCount = frameCount,
      headFeatures = headFeatures,
      forkOrNull = forkOrNull,
    )

  val accumFeatures: Features = if isTailless then headFeatures else headFeatures | tail.accumFeatures
  val fork: Stack = if forkOrNull != null then forkOrNull else this

  def nextPromptIndex: Int = piles.size
  // def segmentIsEmpty: Boolean = piles.isEmpty
  def isTailless: Boolean = tailOrNull == null
  def tail: Stack = tailOrNull.nn
  def canPop: Boolean = if isTailless then frameCount > 1 else true


  def makeFork: Stack = if isTailless then fork else fork.copy(tailOrNull = tail.makeFork)


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
      cache.interpreter = prompt.interpreter
      cache.prompt = prompt //@#@TODO obsolete
      cache.location = location.withDepth(depth)
    else
      if isTailless then
        sigNotFound(sig)
      else
        tail.locateSignature(sig, cache, depth + 1)


  def locatePrompt(interp: Interpreter.Untyped, cache: Cache): Unit =
    locateSignature(interp.signatures.head, cache)


  def locateIO: Location.Deep =
    val sig = Prompt.io.signatures.head
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


  //--------- from StackSegment ----------

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


  def pushBase(loc: Location.Shallow, step: Step, prompt: Prompt): Stack =
    //// shared {{
    val newSignatures = signatures ++ prompt.signatures
    val newLocations = locations ++ Array.fill(prompt.signatures.size)(loc)
    val newPrompts = prompts :+ prompt
    //// }}
    val newFork =
      val pile1 = Pile.base(loc.promptIndex)
      fork.pushBaseWithFork(pile1, prompt, newSignatures, newLocations, newPrompts, forkOrNull = null)
    val pile2 = Pile.pushFirst(step, height = frameCount, isNested = false, FrameKind.plain)
    pushBaseWithFork(pile2, prompt, newSignatures, newLocations, newPrompts, forkOrNull = newFork)


  private def pushBaseWithFork(
    pile: Pile,
    prompt: Prompt,
    newSignatures: Array[Signature],
    newLocations: Array[Location.Shallow],
    newPrompts: Array[Prompt],
    forkOrNull: Stack | Null,
  ): Stack =
    copy(
      signatures = newSignatures,
      locations = newLocations,
      prompts = newPrompts,
      piles = piles :+ pile,
      frameCount = frameCount + 1,
      headFeatures = headFeatures | prompt.features.mask,
      forkOrNull = forkOrNull,
    )


  def popLast(prompt: Prompt): Stack =
    assert(prompt == prompts.last)
    //@#@OPTY share components when possible
    if piles.last.hasBase then
      val newFork = fork.popLastWithFork(prompt, null)
      popLastWithFork(prompt, newFork)
    else
      popLastWithFork(prompt, fork)


  private def popLastWithFork(prompt: Prompt, forkOrNull: Stack | Null): Stack =
    val newPrompts = prompts.init
    val newSigCount = signatures.size - prompt.signatures.size
    copy(
      signatures = signatures.take(newSigCount),
      locations = locations.take(newSigCount),
      prompts = newPrompts,
      piles = piles.init,
      frameCount = frameCount - 1,
      headFeatures = newPrompts.iterator.map(_.features).reduce(_ | _), //@#@OPTY
      forkOrNull = forkOrNull,
    )


  def pushNextNested(loc: Location.Shallow, step: Step, savedLocal: Local, kind: FrameKind): Stack =
    val oldPile = piles(loc.promptIndex)
    val newPile = oldPile.pushNested(step, savedLocal, height = frameCount, kind)
    copy(
      piles = piles.updated(loc.promptIndex, newPile),
      frameCount = frameCount + 1,
      forkOrNull = fork,
    )


  def popNextNested(loc: Location.Shallow): Stack =
    val oldPile = piles(loc.promptIndex)
    val newPile = oldPile.pop
    copy(
      piles = piles.updated(loc.promptIndex, newPile),
      frameCount = frameCount - 1,
      forkOrNull = fork,
    )


  def ::?(that: (Stack | Null, Step | Null)): Stack =
    copy(tailOrNull = that._1, aside = that._2.asInstanceOf[Step])

  //@#@TODO use
  def copyTailless: Stack = copy(tailOrNull = null, aside = null, forkOrNull = null)

  //@#@TODO use
  def setTailInPlace(tailOrNull: Stack | Null, aside: Step | Null): Unit =
    this.tailOrNull = tailOrNull
    this.aside = aside


  final def toStr: String = s"Stack(${toStrAux})"


  final def toStrAux: String =
    if this == null.asInstanceOf[Stack] then
      "NULL"
    else
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


private object Stack:
  val initial: Stack =
    pushFirst(
      tailOrNull = null,
      aside = null,
      prompt = Prompt.io,
      isNested = false,
      kind = FrameKind.plain,
    )


  private val emptyFork: Stack =
    new Stack(
      tailOrNull = null,
      aside = Step.Pop,
      signatures = Array(),
      locations = Array(),
      prompts = Array(),
      piles = Array(),
      frameCount = 0,
      headFeatures = Features.Empty,
      forkOrNull = null,
    )


  def pushFirst(
    tailOrNull: Stack | Null,
    aside: Step | Null,
    prompt: Prompt,
    isNested: Boolean,
    kind: FrameKind,
  ): Stack =
    val newStack =
      val newPile = Pile.pushFirst(StepCases.Pop, 0, isNested, kind)
      val newLoc = Location.Shallow(0, 0, prompt.isStateful)
      new Stack(
        tailOrNull = tailOrNull,
        aside = aside,
        signatures = prompt.signatures,
        locations = Array.fill(prompt.signatures.size)(newLoc),
        prompts = Array(prompt),
        piles = Array(newPile),
        frameCount = 1,
        headFeatures = prompt.features.mask,
        forkOrNull = if isNested then emptyFork else null,
      )
    if isNested then
      newStack
    else
      newStack.copy(forkOrNull = newStack)


  def blank(
    tailOrNull: Stack | Null,
    aside: Step | Null,
    sigCount: Int,
    promptCount: Int,
    frameCount: Int,
    headFeatures: Features,
    forkOrNull: Stack | Null
  ): Stack =
    new Stack(
      tailOrNull = tailOrNull,
      aside = aside,
      signatures = new Array(sigCount),
      locations = new Array(sigCount),
      prompts = new Array(promptCount),
      piles = new Array(promptCount),
      frameCount = frameCount,
      headFeatures = headFeatures,
      forkOrNull = forkOrNull,
    )

package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.Signature
import turbolift.interpreter.Features


private[engine] final class StackSegment private (
  val signatures: Array[Signature],
  val locations: Array[Location.Shallow],
  val prompts: Array[Prompt],
  val piles: Array[Pile],
  val frameCount: Int,
  override val features: Features,
  forkOrNull: StackSegment | Null,
) extends Stack:
  override val head: StackSegment = this
  val fork: StackSegment = if forkOrNull != null then forkOrNull else this


  private def copy(
    signatures: Array[Signature] = signatures,
    locations: Array[Location.Shallow] = locations,
    prompts: Array[Prompt] = prompts, 
    piles: Array[Pile] = piles,
    frameCount: Int = frameCount,
    features: Features = features,
    forkOrNull: StackSegment | Null,
  ): StackSegment = new StackSegment(
    signatures = signatures,
    locations = locations,
    prompts = prompts, 
    piles = piles,
    frameCount = frameCount,
    features = features,
    forkOrNull = forkOrNull,
  )


  override def toString =
    val aa = 
      (for i <- 0.until(prompts.size) yield
        s"${prompts(i)}:${piles(i)}"
      ).reverse.mkString(", ")
    s"[$aa]"


  def isSameAsItsFork: Boolean = this == fork
  def size: Int = piles.size
  def isEmpty: Boolean = piles.isEmpty
  def asStack: Stack = this


  inline def ::?(inline that: (Stack | Null, Step | Null)): Stack =
    val (stack, step) = that
    if stack == null then
      asStack
    else
      StackNel(this, stack, step.nn).asStack


  def locateHighestPile: Location.Shallow =
    //@#@OPTY search in reverse order
    val n = frameCount - 1
    @tailrec def loop(i: Int, j: Int): Location.Shallow =
      val pile = piles(i)
      val prompt = prompts(i)
      if pile.maxHeight == n then
        Location.Shallow(promptIndex = i, stanIndex = j, isStateful = prompt.isStateful)
      else
        loop(i + 1, j + prompt.stanCount)
    loop(0, 0)


  def pushBase(loc: Location.Shallow, step: Step, prompt: Prompt): StackSegment =
    //@#@OPTY share components when possible
    val newFork = fork.pushBaseWithFork(loc, StepCases.Pop, prompt, Pile.base(loc.promptIndex), null)
    val newPile = Pile.pushFirst(step, height = frameCount, isLocal = false, FrameKind.plain)
    pushBaseWithFork(loc, step, prompt, newPile, newFork)


  private def pushBaseWithFork(loc: Location.Shallow, step: Step, prompt: Prompt, pile: Pile, forkOrNull: StackSegment | Null): StackSegment =
    new StackSegment(
      signatures = signatures ++ prompt.signatures,
      locations = locations ++ Array.fill(prompt.signatures.size)(loc),
      prompts = prompts :+ prompt,
      piles = piles :+ pile,
      frameCount = frameCount + 1,
      features = features | prompt.features.mask,
      forkOrNull = forkOrNull,
    )


  def popLast(prompt: Prompt): StackSegment =
    assert(prompt == prompts.last)
    //@#@OPTY share components when possible
    if piles.last.hasBase then
      val newFork = fork.popLastWithFork(prompt, null)
      popLastWithFork(prompt, newFork)
    else
      popLastWithFork(prompt, fork)


  private def popLastWithFork(prompt: Prompt, forkOrNull: StackSegment | Null): StackSegment =
    val newPrompts = prompts.init
    val newSigCount = signatures.size - prompt.signatures.size
    new StackSegment(
      signatures = signatures.take(newSigCount),
      locations = locations.take(newSigCount),
      prompts = newPrompts,
      piles = piles.init,
      frameCount = frameCount - 1,
      features = newPrompts.iterator.map(_.features).reduce(_ | _), //@#@OPTY
      forkOrNull = forkOrNull,
    )


  def pushNextLocal(loc: Location.Shallow, step: Step, savedStan: Stan, kind: FrameKind): StackSegment =
    val oldPile = piles(loc.promptIndex)
    val newPile = oldPile.pushLocal(step, savedStan, height = frameCount, kind)
    copy(
      piles = piles.updated(loc.promptIndex, newPile),
      frameCount = frameCount + 1,
      forkOrNull = fork,
    )


  def popNextLocal(loc: Location.Shallow): StackSegment =
    val oldPile = piles(loc.promptIndex)
    val newPile = oldPile.pop
    copy(
      piles = piles.updated(loc.promptIndex, newPile),
      frameCount = frameCount - 1,
      forkOrNull = fork,
    )


private[engine] object StackSegment:
  private val emptyFork: StackSegment =
    new StackSegment(
      signatures = Array(),
      locations = Array(),
      prompts = Array(),
      piles = Array(),
      frameCount = 0,
      features = Features.Empty,
      forkOrNull = null,
    )


  val initial: StackSegment =
    pushFirst(Prompt.io, isLocal = false, kind = FrameKind.plain)


  def pushFirst(prompt: Prompt, isLocal: Boolean, kind: FrameKind): StackSegment =
    val newSeg =
      val newPile = Pile.pushFirst(StepCases.Pop, 0, isLocal, kind)
      val newLoc = Location.Shallow(0, 0, prompt.isStateful)
      new StackSegment(
        signatures = prompt.signatures,
        locations = Array.fill(prompt.signatures.size)(newLoc),
        prompts = Array(prompt),
        piles = Array(newPile),
        frameCount = 1,
        features = prompt.features.mask,
        forkOrNull = if isLocal then emptyFork else null,
      )
    if isLocal then
      newSeg
    else
      newSeg.copy(forkOrNull = newSeg)


  def blank(sigCount: Int, promptCount: Int, frameCount: Int, features: Features, forkOrNull: StackSegment | Null): StackSegment =
    new StackSegment(
      signatures = new Array(sigCount),
      locations = new Array(sigCount),
      prompts = new Array(promptCount),
      piles = new Array(promptCount),
      frameCount = frameCount,
      features = features,
      forkOrNull = forkOrNull,
    )

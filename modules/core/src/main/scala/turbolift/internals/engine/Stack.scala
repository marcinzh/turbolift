package turbolift.internals.engine
import scala.annotation.{tailrec, switch}
import turbolift.!!
import turbolift.internals.interpreter.Void
import turbolift.internals.engine.{StepCases => SC, HistoryCases => HC}


private[engine] final class Stack(
  val lookup: Lookup,
  val history: History,
  val config: Config,
  val levels: Array[Level],
  val lastHeight: Int,
  forkOrNull: Stack | Null,
):
  def copy(
    lookup: Lookup = lookup,
    history: History = history,
    config: Config = config,
    levels: Array[Level] = levels,
    lastHeight: Int = lastHeight,
    forkOrNull: Stack | Null = fork,
  ): Stack = new Stack(
    lookup = lookup,
    history = history,
    config = config,
    levels = levels,
    lastHeight = lastHeight,
    forkOrNull = forkOrNull,
  )

  val fork: Stack = if forkOrNull != null then forkOrNull else this 

  assert(lookup == fork.lookup)
  assert(levels.size == fork.levels.size)
  assert(fork.fork == fork)
  assert(fork.lastHeight + 1 == fork.levels.size)

  override def toString = s"Stack{^${lastHeight} ${levels.toVector.reverse.mkString(", ")}}"


  def canPopFlow: Boolean = lastHeight >= 0
  def nextLevelIndex = levels.size
  def promptCount = levels.size
  def promptAt(i: Int) = levels(i).prompt
  
  def getLevelAt(prompt: Prompt): Level =
    val l = levels(prompt.levelIndex)
    assert(l.prompt == prompt)
    l

  def getTopSegmentAt(prompt: Prompt): Segment = getLevelAt(prompt).segments.head

  def pushFlow(prompt: Prompt, step: Step): Stack =
    val newLookup = lookup.push(prompt)
    val newForkStack = fork.pushFlow1(prompt, newLookup, SC.Done, HC.Empty, null)
    val newMainStack = this.pushFlow1(prompt, newLookup, step, history, newForkStack)
    newMainStack

  private def pushFlow1(prompt: Prompt, newLookup: Lookup, step: Step, oldHistory: History, newForkOrNull: Stack | Null): Stack =
    val newHeight = lastHeight + 1
    val newLevels =
      val newFrame = Frame.base(newHeight)
      val newSegment = new Segment(newFrame, Void, step, oldHistory)
      val newLevel = new Level(prompt, newSegment :: Nil, lookup)
      levels :+ newLevel
    copy(
      lookup = newLookup,
      history = HC.Empty,
      levels = newLevels,
      lastHeight = newHeight,
      forkOrNull = newForkOrNull,
    )

  def pushLocal(prompt: Prompt, step: Step, history: History, oldStan: Any): Stack =
    val newHeight = lastHeight + 1
    val newFrame = Frame.local(newHeight)
    val newSegment = new Segment(newFrame, Void, step, history)
    val oldLevel = getLevelAt(prompt)
    val newLevel = oldLevel.copy(segments = newSegment :: oldLevel.segments.patch(oldStan))
    val newLevels = levels.updated(prompt.levelIndex, newLevel)
    copy(levels = newLevels, lastHeight = newHeight)

  def popFlow(oldStore: Store): (Step, Stack, Store, Prompt, Any) =
    val newHeight = lastHeight - 1
    val i = levels.indexWhere(l => l.maxHeight == lastHeight)
    val topLevel = levels(i)
    val topSegment = topLevel.top
    val prompt = topLevel.prompt
    val topStan = oldStore.getOrElseVoid(prompt)
    if topSegment.frame.isBase then
      assert(i == levels.size - 1)
      val newStore = oldStore.dropIfHasStan(prompt)
      val that = copy(
        lookup = topLevel.savedLookup,
        history = topSegment.history,
        levels = levels.init,
        lastHeight = newHeight,
        forkOrNull = if fork == this then null else fork.popFlowFork(topLevel.savedLookup),
      )
      (topSegment.step, that, newStore, prompt, topStan)
    else
      val deepSegments = topLevel.segments.tail
      val newStore = oldStore.setIfNotVoid(prompt, deepSegments.head.savedStan)
      val newLevels = 
        val newLevel = topLevel.copy(segments = deepSegments.unpatch)
        levels.updated(i, newLevel)
      val that = copy(
        history = topSegment.history,
        levels = newLevels,
        lastHeight = newHeight,
      )
      (topSegment.step, that, newStore, prompt, topStan)

  private def popFlowFork(savedLookup: Lookup): Stack =
    assert(fork == this)
    copy(
      lookup = savedLookup,
      levels = levels.init,
      forkOrNull = null,
      lastHeight = lastHeight - 1,
    )

  def pushProxy(prompt: Prompt, step: Step): Stack =
    val newLookup = lookup.push(prompt)
    val newFork = fork.copy(lookup = newLookup, forkOrNull = null)
    copy(
      lookup = newLookup,
      history = new HC.Proxied(step, history, lookup),
      forkOrNull = newFork,
    )

  def popProxy(savedLookup: Lookup, nextHistory: History): Stack =
    val newFork = fork.copy(lookup = savedLookup, forkOrNull = null)
    copy(
      lookup = savedLookup,
      history = nextHistory,
      forkOrNull = newFork,
    )

  def pushConfig(newConfig: Config, step: Step): Stack =
    val newFork = fork.copy(config = newConfig, forkOrNull = null)
    copy(
      history = new HC.Configured(step, history, config),
      config = newConfig,
      forkOrNull = newFork,
    )

  def popConfig(savedConfig: Config, nextHistory: History): Stack =
    val newFork = fork.copy(config = savedConfig, forkOrNull = null)
    copy(
      config = savedConfig,
      history = nextHistory,
      forkOrNull = newFork,
    )

  private def splice(divSegment: Segment, kont: Kont, oldStore: Store): (Stack, Store) =
    Stack.splice(
      divPrompt = kont.prompt,
      divSegment = divSegment,
      stackHi = kont.stack,
      stackLo = this,
      storeHi = kont.store,
      storeLo = oldStore,
    )

  def spliceForRestore(deepStep: Step, kont: Kont, oldStore: Store): (Stack, Store) =
    val divSegment =
      kont.stack.getTopSegmentAt(kont.prompt).copy(
        step = deepStep,
        history = history,
        savedStan = kont.getStan,
      )
    splice(divSegment, kont, oldStore)

  def spliceForResume(stepAside: Step, kont: Kont, oldStore: Store, newStan: Any): (Stack, Store) =
    val divSegment = 
      val s = Void.orElse(newStan, kont.getStan)
      val oldSegment = kont.getDivSegment
      val repeatStep = new SC.Restore(stepAside, kont, oldSegment.step)
      oldSegment.copy(step = repeatStep, savedStan = s)
    splice(divSegment, kont, oldStore)

  def spliceForLocal(stepAside: Step, kont: Kont, oldStore: Store, newStan: Any): (Stack, Store) =
    val divSegment =
      val s = Void.orElse(newStan, kont.getStan)
      getTopSegmentAt(kont.prompt).patch(s)
    val (newStack, newStore) = splice(divSegment, kont, oldStore)
    val newStack2 =
      val oldStan = oldStore.getOrElseVoid(kont.prompt)
      val captureStep = SC.Capture(kont.prompt, stepAside, kont.step)
      newStack.pushLocal(kont.prompt, captureStep, HC.Empty, oldStan)
    val newStore2 = newStore.setIfNotVoid(kont.prompt, newStan)
    (newStack2, newStore2)



private[engine] object Stack:
  def initial(config: Config): Stack =
    new Stack(
      lookup = Lookup.initial,
      history = HC.Empty,
      config = config,
      levels = Array[Level](),
      lastHeight = -1,
      forkOrNull = null
    )

  //// Behold: the absolute most PITA part of this project.
  //// Currently left unoptimized, due to fear of breaking it.
  def splice(
    divPrompt: Prompt,
    divSegment: Segment,
    stackHi: Stack,
    stackLo: Stack,
    storeHi: Store,
    storeLo: Store,
  ): (Stack, Store) =
    val sizeHi = stackHi.levels.size
    val sizeLo = stackLo.levels.size
    val divHeight = divSegment.height
    val newLevels = new Array[Level](sizeHi)
    val newStore = storeHi.blank
    def loop(i: Int): Unit =
      if i < sizeHi then
        val levelHi = stackHi.levels(i)
        val currPrompt = levelHi.prompt

        val segmentsLo =
          if i < sizeLo then
            val levelLo = stackLo.levels(i)
            assert(currPrompt == levelLo.prompt)
            val stanLo = storeLo.getOrElseVoid(currPrompt)
            levelLo.segments.patch(stanLo).dropWhile(_.height >= divHeight)
          else
            Nil

        val segmentsHi =
          if i == divPrompt.levelIndex then
            assert(levelHi.segments.head.height == divHeight)
            divSegment :: Nil
          else
            val stanHi = storeHi.getOrElseVoid(currPrompt)
            levelHi.segments.patch(stanHi).takeWhile(_.height >= divHeight)

        val segmentsBoth = segmentsHi ++ segmentsLo
        val newStan = segmentsBoth.head.savedStan
        val newSegments = segmentsBoth.unpatch
        newLevels(i) = levelHi.copy(segments = newSegments)
        newStore.setInPlaceIfHasStan(levelHi.prompt, newStan)
        loop(i + 1)
    loop(0)
    val newStack = stackHi.copy(levels = newLevels)
    (newStack, newStore)

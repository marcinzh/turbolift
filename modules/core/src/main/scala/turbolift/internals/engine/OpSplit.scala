package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.interpreter.Features


private[engine] object OpSplit:
  inline def forceSplitAndThen[T](stack: Stack, store: Store, mark: Mark)(inline cb: (Stack, Store) => T): T =
    val prompt = mark.unwrap
    if prompt == null then
      cb(stack, store)
    else
      val (stack2, store2, _) = splitLo(stack, store, prompt)
      cb(stack2, store2)


  def splitLo(stack: Stack, store: Store, prompt: Prompt): (Stack, Store, Step) =
    @tailrec def loop(todoStack: Stack, todoStore: Store): (Stack, Store, Step) =
      todoStack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
        todoStore.deconsAndThen: (oldStoreSeg, moreStore) =>
          val promptIndex = oldStackSeg.prompts.indexOf(prompt)
          if promptIndex >= 0 then
            val divPile = oldStackSeg.piles(promptIndex)
            val divHeight = divPile.maxHeight
            if divHeight == 0 then
              //// Fast path: stack has already been split at this prompt
              (moreStack.nn, moreStore.nn, moreStep.nn)
            else
              //// Slow path: split this segment
              val divStep = divPile.topFrame.step
              val newFrameCount = divHeight
              val (newStackSeg, newStoreSeg) = migrate(oldStackSeg, oldStoreSeg, newFrameCount, _.splitLo(divHeight, _))
              val newStack = newStackSeg ::? (moreStack, moreStep)
              val newStore = newStoreSeg ::? moreStore
              (newStack, newStore, divStep)
          else
            loop(
              todoStack = moreStack.nn,
              todoStore = moreStore.nn,
            )
    loop(stack, store)


  def splitHi(stack: Stack, store: Store, location: Location.Deep): (Stack, Store) =
    @tailrec def loop(
      todoStack: Stack,
      todoStore: Store,
      depth: Int,
      accumStack: Stack | Null,
      accumStore: Store | Null,
      accumStep: Step | Null
    ): (Stack, Store) =
      todoStack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
        todoStore.deconsAndThen: (oldStoreSeg, moreStore) =>
          if depth == 0 then
            val divHeight = oldStackSeg.piles(location.promptIndex).maxHeight
            if divHeight == 0 then
              //// Fast path: stack has already been split at this location
              val newStack = oldStackSeg ::? (accumStack, accumStep)
              val newStore = oldStoreSeg ::? accumStore
              (newStack, newStore)
            else
              //// Slow path: split this segment
              val newFrameCount = oldStackSeg.frameCount - divHeight
              val (newStackSeg, newStoreSeg) = migrate(oldStackSeg, oldStoreSeg, newFrameCount, _.splitHi(divHeight, _))
              val newStack = newStackSeg ::? (accumStack, accumStep)
              val newStore = newStoreSeg ::? accumStore
              (newStack, newStore)
          else
            loop(
              todoStack = moreStack.nn,
              todoStore = moreStore.nn,
              depth = depth - 1,
              accumStack = oldStackSeg ::? (accumStack, accumStep),
              accumStore = oldStoreSeg ::? accumStore,
              accumStep = moreStep.nn,
            )
    loop(stack, store, location.segmentDepth, null, null, null)


  private def migrate(
    oldStackSeg: StackSegment,
    oldStoreSeg: StoreSegment,
    newFrameCount: Int,
    pileSplitter: PileSplitter,
  ): (StackSegment, StoreSegment) =
    val (newStackSeg, newStoreSeg, bundles, newPromptCount) = migrateOut(oldStackSeg, oldStoreSeg, newFrameCount, pileSplitter)
    sort(bundles, newPromptCount)
    migrateIn(newStackSeg, newStoreSeg, bundles, newPromptCount)
    (newStackSeg, newStoreSeg)


  private type PileSplitter = (Pile, Stan) => (Pile | Null, Stan)

  private case class Bundle(prompt: Prompt, pile: Pile, stan: Stan):
    def ord = pile.minHeight


  private def sort(arr: Array[Bundle], n: Int): Unit =
    var i: Int = 1
    while i < n do
      val a = arr(i)
      @tailrec def loop(j: Int): Int =
        if j > 0 then
          val b = arr(j - 1)
          if a.ord < b.ord then
            arr(j) = b
            loop(j - 1)
          else
            j
        else
          0
      arr(loop(i)) = a
      i += 1


  private def migrateOut(
    oldStackSeg: StackSegment,
    oldStoreSeg: StoreSegment,
    newFrameCount: Int,
    pileSplitter: PileSplitter,
  ): (StackSegment, StoreSegment, Array[Bundle], Int) =
    val oldPromptCount = oldStackSeg.piles.size
    val bundles = new Array[Bundle](oldPromptCount)

    var srcPromptIndex: Int = 0
    var srcStanIndex: Int = 0
    var newPromptCount: Int = 0
    var newStanCount: Int = 0
    var newSigCount: Int = 0
    var newFeatures = Features.Empty
    var newForkPromptCount: Int = 0
    var newForkSigCount: Int = 0
    var newForkFeatures = Features.Empty

    while srcPromptIndex < oldPromptCount do
      val prompt = oldStackSeg.prompts(srcPromptIndex)
      val oldPile = oldStackSeg.piles(srcPromptIndex)
      val oldStan = if prompt.isStateless then Stan.void else oldStoreSeg.geti(srcStanIndex)
      val (newPile, newStan) = pileSplitter(oldPile, oldStan)
      if newPile != null then
        bundles(newPromptCount) = Bundle(prompt, newPile, newStan)
        newPromptCount += 1
        newSigCount += prompt.signatures.size
        newStanCount += prompt.stanCount
        newFeatures |= prompt.features.mask
        if newPile.hasBase then
          newForkPromptCount += 1
          newForkSigCount += prompt.signatures.size
          newForkFeatures |= prompt.features.mask
      srcPromptIndex += 1
      srcStanIndex += prompt.stanCount
    
    val newStackSeg =
      val newFork = StackSegment.blank(
        sigCount = newForkSigCount,
        promptCount = newForkPromptCount,
        frameCount = newForkPromptCount, //// 1 frame in each pile
        features = newForkFeatures,
        forkOrNull = null,
      )
      StackSegment.blank(
        sigCount = newSigCount,
        promptCount = newPromptCount,
        frameCount = newFrameCount,
        features = newFeatures,
        forkOrNull = newFork,
      )
    //@#@OPTY reuse `oldStoreSeg` if number of elements stays the same
    val newStoreSeg = StoreSegment.blank(newStanCount)
    (newStackSeg, newStoreSeg, bundles, newPromptCount)


  private def migrateIn(
    newStackSeg: StackSegment,
    newStoreSeg: StoreSegment,
    bundles: Array[Bundle],
    newPromptCount: Int,
  ): Unit =
    var promptIndex: Int = 0
    var destStanIndex: Int = 0
    var destSigIndex: Int = 0
    var destForkPromptIndex: Int = 0
    var destForkStanIndex: Int = 0
    var destForkSigIndex: Int = 0
    while promptIndex < newPromptCount do
      val bundle = bundles(promptIndex)
      val prompt = bundle.prompt
      addPromptInPlace(prompt, bundle.pile, newStackSeg, promptIndex, destStanIndex, destSigIndex)
      if prompt.isStateful then
        newStoreSeg.setInPlace(destStanIndex, bundle.stan)
      if bundle.pile.hasBase then
        val forkPile = Pile.base(destForkPromptIndex)
        addPromptInPlace(prompt, forkPile, newStackSeg.fork, destForkPromptIndex, destForkStanIndex, destForkSigIndex)
        destForkPromptIndex += 1
        destForkSigIndex += prompt.signatures.size
        destForkStanIndex += prompt.stanCount
      promptIndex += 1
      destSigIndex += prompt.signatures.size
      destStanIndex += prompt.stanCount


  private def addPromptInPlace(
    prompt: Prompt,
    pile: Pile,
    destStackSeg: StackSegment,
    destPromptIndex: Int,
    destStanIndex: Int,
    destSigIndex: Int,
  ): Unit =
    destStackSeg.prompts(destPromptIndex) = prompt
    destStackSeg.piles(destPromptIndex) = pile
    val loc = Location.Shallow(
      promptIndex = destPromptIndex,
      stanIndex = destStanIndex,
      isStateful = prompt.isStateful,
    )
    val sigs = prompt.signatures
    var i = 0
    while i < sigs.size do
      destStackSeg.signatures(destSigIndex + i) = sigs(i)
      destStackSeg.locations(destSigIndex + i) = loc
      i += 1


  def merge(
    stackHi: Stack,
    storeHi: Store,
    stepMid: Step,
    //@#@OPTY fuse with `store.setIfNotVoid``
    // stanMid: Stan,
    // locationMid: Location.Shallow,
    stackLo: Stack,
    storeLo: Store,
  ): (Stack, Store) =
    @tailrec def loop(
      todoStack: Stack,
      todoStore: Store,
      lastStep: Step,
      accumStack: Stack,
      accumStore: Store,
    ): (Stack, Store) =
      todoStack.deconsAndThen: (stackSeg, moreStack, moreStep) =>
        todoStore.deconsAndThen: (storeSeg, moreStore) =>
          val newStack = stackSeg ::? (accumStack, lastStep)
          val newStore = storeSeg ::? accumStore
          if moreStack == null then
            (newStack, newStore)
          else
            loop(
              todoStack = moreStack.nn,
              todoStore = moreStore.nn,
              lastStep = moreStep.nn,
              accumStack = newStack,
              accumStore = newStore,
            )
    loop(
      todoStack = stackHi,
      todoStore = storeHi,
      lastStep = stepMid,
      accumStack = stackLo,
      accumStore = storeLo,
    )

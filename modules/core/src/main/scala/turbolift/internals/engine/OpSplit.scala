package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.interpreter.Features


private object OpSplit:
  def split(stack: Stack, store: Store, location: Location.Deep): (Stack, Store, Step, Stack, Store) =
    @tailrec def loop(
      todoStack: Stack,
      todoStore: Store,
      accumStack: Stack | Null,
      accumStore: Store | Null,
      accumStep: Step | Null,
      depth: Int,
    ): (Stack, Store, Step, Stack, Store) =
      todoStack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
        todoStore.deconsAndThen: (oldStoreSeg, moreStore) =>
          if depth == 0 then
            val divPile = oldStackSeg.piles(location.promptIndex)
            val divHeight = divPile.maxHeight
            if divHeight == 0 then
              //// Fast path: stack has already been split at this location
              val newStackHi = oldStackSeg ::? (accumStack, accumStep)
              val newStoreHi = oldStoreSeg ::? accumStore
              (newStackHi, newStoreHi, moreStep.nn, moreStack.nn, moreStore.nn)
            else
              //// Slow path: split this segment
              val frameCountHi = oldStackSeg.frameCount - divHeight
              val frameCountLo = divHeight
              //@#@OPTY do hi & lo in one go
              val (newStackSegHi, newStoreSegHi) = migrate(oldStackSeg, oldStoreSeg, frameCountHi, _.splitHi(divHeight, _))
              val (newStackSegLo, newStoreSegLo) = migrate(oldStackSeg, oldStoreSeg, frameCountLo, _.splitLo(divHeight, _))
              val newStackHi = newStackSegHi ::? (accumStack, accumStep)
              val newStoreHi = newStoreSegHi ::? accumStore
              val newStackLo = newStackSegLo ::? (moreStack, moreStep)
              val newStoreLo = newStoreSegLo ::? moreStore
              val stepMid = divPile.topFrame.step
              (newStackHi, newStoreHi, stepMid, newStackLo, newStoreLo)
          else
            loop(
              todoStack = moreStack.nn,
              todoStore = moreStore.nn,
              accumStack = oldStackSeg ::? (accumStack, accumStep),
              accumStore = oldStoreSeg ::? accumStore,
              accumStep = moreStep.nn,
              depth = depth - 1,
            )
    loop(stack, store, null, null, null,  location.segmentDepth)


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


  private type PileSplitter = (Pile, Local) => (Pile | Null, Local)

  private case class Bundle(prompt: Prompt, pile: Pile, local: Local):
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
    var srcLocalIndex: Int = 0
    var newPromptCount: Int = 0
    var newLocalCount: Int = 0
    var newSigCount: Int = 0
    var newFeatures = Features.Empty
    var newForkPromptCount: Int = 0
    var newForkSigCount: Int = 0
    var newForkFeatures = Features.Empty

    while srcPromptIndex < oldPromptCount do
      val prompt = oldStackSeg.prompts(srcPromptIndex)
      val oldPile = oldStackSeg.piles(srcPromptIndex)
      val oldLocal = if prompt.isStateless then Local.void else oldStoreSeg.geti(srcLocalIndex)
      val (newPile, newLocal) = pileSplitter(oldPile, oldLocal)
      if newPile != null then
        bundles(newPromptCount) = Bundle(prompt, newPile, newLocal)
        newPromptCount += 1
        newSigCount += prompt.signatures.size
        newLocalCount += prompt.localCount
        newFeatures |= prompt.features.mask
        if newPile.hasBase then
          newForkPromptCount += 1
          newForkSigCount += prompt.signatures.size
          newForkFeatures |= prompt.features.mask
      srcPromptIndex += 1
      srcLocalIndex += prompt.localCount
    
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
    val newStoreSeg = StoreSegment.blank(newLocalCount)
    (newStackSeg, newStoreSeg, bundles, newPromptCount)


  private def migrateIn(
    newStackSeg: StackSegment,
    newStoreSeg: StoreSegment,
    bundles: Array[Bundle],
    newPromptCount: Int,
  ): Unit =
    var promptIndex: Int = 0
    var destLocalIndex: Int = 0
    var destSigIndex: Int = 0
    var destForkPromptIndex: Int = 0
    var destForkLocalIndex: Int = 0
    var destForkSigIndex: Int = 0
    while promptIndex < newPromptCount do
      val bundle = bundles(promptIndex)
      val prompt = bundle.prompt
      addPromptInPlace(prompt, bundle.pile, newStackSeg, promptIndex, destLocalIndex, destSigIndex)
      if prompt.isStateful then
        newStoreSeg.setInPlace(destLocalIndex, bundle.local)
      if bundle.pile.hasBase then
        val forkPile = Pile.base(destForkPromptIndex)
        addPromptInPlace(prompt, forkPile, newStackSeg.fork, destForkPromptIndex, destForkLocalIndex, destForkSigIndex)
        destForkPromptIndex += 1
        destForkSigIndex += prompt.signatures.size
        destForkLocalIndex += prompt.localCount
      promptIndex += 1
      destSigIndex += prompt.signatures.size
      destLocalIndex += prompt.localCount


  private def addPromptInPlace(
    prompt: Prompt,
    pile: Pile,
    destStackSeg: StackSegment,
    destPromptIndex: Int,
    destLocalIndex: Int,
    destSigIndex: Int,
  ): Unit =
    destStackSeg.prompts(destPromptIndex) = prompt
    destStackSeg.piles(destPromptIndex) = pile
    val loc = Location.Shallow(
      promptIndex = destPromptIndex,
      localIndex = destLocalIndex,
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
    // localMid: Local,
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

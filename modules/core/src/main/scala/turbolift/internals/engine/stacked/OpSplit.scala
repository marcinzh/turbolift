package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.interpreter.Features
import Prompt.Syntax._


private[engine] object OpSplit:
  def split(stack: Stack, store: Store, location: Location.Deep): (Stack, Store, Step, Stack, Store) =
    @tailrec def loop(
      todoStack: Stack,
      todoStore: Store,
      accumStack: Stack | Null,
      accumStore: Store | Null,
      accumStep: Step | Null,
      depth: Int,
    ): (Stack, Store, Step, Stack, Store) =
      if depth == 0 then
        val divPile = todoStack.piles(location.promptIndex)
        val divHeight = divPile.maxHeight
        if divHeight == 0 then
          //// Fast path: stack has already been split at this location
          val stackHi = todoStack ::? (accumStack, accumStep)
          val storeHi = todoStore ::? accumStore
          val stepMid = todoStack.aside.nn
          val stackLo = todoStack.tail
          val storeLo = todoStore.tail
          (stackHi, storeHi, stepMid, stackLo, storeLo)
        else
          //// Slow path: split this segment
          val frameCountHi = todoStack.frameCount - divHeight
          val frameCountLo = divHeight
          //@#@OPTY {{ do hi & lo in one go
          val (stackHi, storeHi) = migrate(todoStack, todoStore, frameCountHi, _.splitHi(divHeight, _))
          val (stackLo, storeLo) = migrate(todoStack, todoStore, frameCountLo, _.splitLo(divHeight, _))
          //@#@ }}
          stackHi.setTailInPlace(accumStack, accumStep)
          storeHi.setTailInPlace(accumStore)
          stackLo.setTailInPlace(todoStack.tailOrJoin, todoStack.aside)
          storeLo.setTailInPlace(todoStore.tailOrNull)
          val stepMid = divPile.topFrame.step
          (stackHi, storeHi, stepMid, stackLo, storeLo)
      else
        loop(
          todoStack = todoStack.tail,
          todoStore = todoStore.tail,
          accumStack = todoStack ::? (accumStack, accumStep),
          accumStore = todoStore ::? accumStore,
          accumStep = todoStack.aside,
          depth = depth - 1,
        )
    loop(stack, store, null, null, null, location.segmentDepth)


  private def migrate(
    oldStack: Stack,
    oldStore: Store,
    newFrameCount: Int,
    pileSplitter: PileSplitter,
  ): (Stack, Store) =
    val (newStack, newStore, bundles, newPromptCount) =
      migrateOut(oldStack, oldStore, newFrameCount, pileSplitter)
    sort(bundles, newPromptCount)
    migrateIn(newStack, newStore, bundles, newPromptCount)
    (newStack, newStore)


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
    oldStack: Stack,
    oldStore: Store,
    newFrameCount: Int,
    pileSplitter: PileSplitter,
  ): (Stack, Store, Array[Bundle], Int) =
    val oldPromptCount = oldStack.piles.size
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
      val oldPile = oldStack.piles(srcPromptIndex)
      val prompt = oldPile.prompt
      val oldLocal = if prompt.isStateless then Local.void else oldStore.geti(srcLocalIndex)
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
    
    val newStack = Stack.blank(
      sigCount = newSigCount,
      promptCount = newPromptCount,
      frameCount = newFrameCount,
      headFeatures = newFeatures,
    )(
      forkSigCount = newForkSigCount,
      forkPromptCount = newForkPromptCount,
      forkHeadFeatures = newForkFeatures,
    )
    //@#@OPTY reuse `oldStore` if number of elements stays the same
    val newStore = oldStore.blankClone(newLocalCount)
    (newStack, newStore, bundles, newPromptCount)


  private def migrateIn(
    newStack: Stack,
    newStore: Store,
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
      addPromptInPlace(prompt, bundle.pile, newStack, promptIndex, destLocalIndex, destSigIndex)
      if prompt.isStateful then
        newStore.setInPlace(destLocalIndex, bundle.local)
      if bundle.pile.hasBase then
        val forkPile = Pile.base(prompt, destForkPromptIndex)
        addPromptInPlace(prompt, forkPile, newStack.fork, destForkPromptIndex, destForkLocalIndex, destForkSigIndex)
        destForkPromptIndex += 1
        destForkSigIndex += prompt.signatures.size
        destForkLocalIndex += prompt.localCount
      promptIndex += 1
      destSigIndex += prompt.signatures.size
      destLocalIndex += prompt.localCount


  private def addPromptInPlace(
    prompt: Prompt,
    pile: Pile,
    destStack: Stack,
    destPromptIndex: Int,
    destLocalIndex: Int,
    destSigIndex: Int,
  ): Unit =
    destStack.piles(destPromptIndex) = pile
    val entry = Entry(prompt, Location.Shallow(promptIndex = destPromptIndex, storeIndex = destLocalIndex))
    val sigs = prompt.signatures
    val lookup = destStack.lookup
    val n = sigs.size
    val i0 = lookup.startIndexForSetInPlace(destSigIndex, n)
    var i = 0
    while i < n do
      lookup.setInPlace(i0, i, sigs(i), entry)
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
      accumStep: Step,
      accumStack: Stack,
      accumStore: Store,
    ): (Stack, Store) =
      val newStack = todoStack ::? (accumStack, accumStep)
      val newStore = todoStore ::? accumStore
      if todoStack.isTailless then
        (newStack, newStore)
      else
        loop(
          todoStack = todoStack.tail,
          todoStore = todoStore.tail,
          accumStep = todoStack.aside.nn,
          accumStack = newStack,
          accumStore = newStore,
        )
    loop(
      todoStack = stackHi,
      todoStore = storeHi,
      accumStep = stepMid,
      accumStack = stackLo,
      accumStore = storeLo,
    )

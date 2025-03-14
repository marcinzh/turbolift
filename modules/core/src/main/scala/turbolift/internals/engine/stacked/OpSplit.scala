package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.interpreter.Features


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
          val (stackHi, storeHi, stackLo, storeLo) = splitSegment(todoStack, todoStore, divHeight)
          stackHi.setTailInPlace(accumStack, accumStep)
          storeHi.setTailInPlace(accumStore)
          stackLo.setTailInPlace(todoStack.tailOrNull, todoStack.asideOrNull)
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


  private def splitSegment(oldStack: Stack, oldStore: Store, divHeight: Int): (Stack, Store, Stack, Store) =
    var promptCountHi = 0
    var promptCountLo = 0
    var sigCountHi = 0
    var sigCountLo = 0
    var localCountHi = 0
    var localCountLo = 0
    var featuresHi = Features.Empty
    var featuresLo = Features.Empty

    //// Temporary bcoz we don't know the sizes of final piles of Hi & Lo Stack yet
    val splitsHi = new Array[Pile.Split1](oldStack.promptCount)
    val splitsLo = new Array[Pile.Split1](oldStack.promptCount)

    //// Precalculate immutable parts of Hi & Lo Stack/Store. Also split piles
    {
      var oldLocalIndex = 0
      val n = oldStack.promptCount
      var i = 0
      while i < n do
        val oldPile = oldStack.piles(i)
        val prompt = oldPile.prompt
        val oldLocal = if prompt.isStateless then Local.void else oldStore.geti(oldLocalIndex)
        val (splitHi, splitLo) = oldPile.split(divHeight, oldLocal)
        if splitHi != null then
          splitsHi(promptCountHi) = splitHi
          promptCountHi += 1
          sigCountHi += prompt.signatures.size
          localCountHi += prompt.localCount
          featuresHi |= prompt.features.mask
        if splitLo != null then
          splitsLo(promptCountLo) = splitLo
          promptCountLo += 1
          sigCountLo += prompt.signatures.size
          localCountLo += prompt.localCount
          featuresLo |= prompt.features.mask
        i += 1
        oldLocalIndex += prompt.localCount
      end while
    }

    val frameCountHi = oldStack.frameCount - divHeight
    val frameCountLo = divHeight
    val newStackHi = Stack.blank(sigCount = sigCountHi, promptCount = promptCountHi, frameCount = frameCountHi, headFeatures = featuresHi)
    val newStackLo = Stack.blank(sigCount = sigCountLo, promptCount = promptCountLo, frameCount = frameCountLo, headFeatures = featuresLo)
    //@#@OPTY reuse `oldStore` if number of elements stays the same
    val newStoreHi = oldStore.blankClone(localCountHi)
    val newStoreLo = oldStore.blankClone(localCountLo)
    fill(splitsHi, newStackHi, newStoreHi)
    fill(splitsLo, newStackLo, newStoreLo)
    (newStackHi, newStoreHi, newStackLo, newStoreLo)


  //// Fills Stack.pile, Stack.lookup and Store
  private def fill(splits: Array[Pile.Split1], newStack: Stack, newStore: Store): Unit =
    val n = newStack.promptCount
    sort(splits, n)
    var i = 0
    var localIndex = 0
    var lookupIndex = newStack.lookup.initialIndexForSetInPlace
    while i < n do
      val split = splits(i)
      newStack.piles(i) = split.pile
      val prompt = split.pile.prompt
      if prompt.isStateful then
        newStore.setInPlace(localIndex, split.local)
      val entry = new Entry(prompt, promptIndex = i, storeIndex = localIndex)
      lookupIndex = newStack.lookup.setInPlace(lookupIndex, entry)
      localIndex += prompt.localCount
      i += 1


  private def sort(arr: Array[Pile.Split1], n: Int): Unit =
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

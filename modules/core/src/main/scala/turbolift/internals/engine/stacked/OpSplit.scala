package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.interpreter.Features


private[engine] object OpSplit:
  def split(stack: Stack, store: Store, location: Location.Deep, truncate: Boolean): (Stack, Store, Step, Stack, Store) =
    @tailrec def loop(
      todoStack: Stack,
      todoStore: Store,
      accumStack: Stack | Null,
      accumStore: Store | Null,
      depth: Int,
    ): (Stack, Store, Step, Stack, Store) =
      if depth == 0 then
        val divPile = todoStack.piles(location.promptIndex)
        val divHeight = divPile.maxHeight
        if divHeight == 0 && (!truncate || todoStack.frameCount == 1) then
          //// Fast path: split happens at the lower boundary of this segment.
          //// Also at the upper boundary, if `truncate`.
          var stackHi: Stack = null.asInstanceOf[Stack]
          var storeHi: Store = null.asInstanceOf[Store]
          val stepMid = todoStack.aside.nn
          val stackLo = todoStack.tail
          val storeLo = todoStore.tail
          if truncate then
            if accumStack == null then
              stackHi = Stack.empty
              storeHi = Store.empty
            else
              stackHi = Stack.empty.copyWithTail(accumStack)
              storeHi = Store.empty.copyWithTail(accumStore)
          else
            //// Clear `aside` bcoz is extracted as `stepMid`
            stackHi = todoStack.copyWithTail2(accumStack, null) 
            storeHi = todoStore.copyWithTail(accumStore)
          (stackHi, storeHi, stepMid, stackLo, storeLo)
        else
          //// Slow path: split this segment
          val stepMid = divPile.topFrame.step
          val (stackHi, storeHi, stackLo, storeLo) = splitSegment(todoStack, todoStore, divHeight, truncate)
          stackHi.setTailInPlace(accumStack)
          storeHi.setTailInPlace(accumStore)
          stackLo.setTailInPlace2(todoStack.tailOrNull, todoStack.asideOrNull)
          storeLo.setTailInPlace(todoStore.tailOrNull)
          //// It's ok when stackHi's head segment is empty, bcoz its dormant (stored in continuation).
          //// But stackLo's head segment must be non empty.
          if stackLo.nonEmptySegment then
            (stackHi, storeHi, stepMid, stackLo, storeLo)
          else
            //// Squash empty segment, but don't lose it's `aside`
            (stackHi, storeHi, stepMid.append(stackLo.aside), stackLo.tail, storeLo.tail)
      else
        loop(
          todoStack = todoStack.tail,
          todoStore = todoStore.tail,
          accumStack = todoStack.copyWithTail(accumStack),
          accumStore = todoStore.copyWithTail(accumStore),
          depth = depth - 1,
        )
    loop(stack, store, null, null, location.segmentDepth)


  private def splitSegment(oldStack: Stack, oldStore: Store, divHeight: Int, truncate: Boolean): (Stack, Store, Stack, Store) =
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
        val (splitHi, splitLo) = oldPile.split(divHeight, oldLocal, truncate)
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

    val frameCountHi = oldStack.frameCount - divHeight - (if truncate then 1 else 0)
    val frameCountLo = divHeight
    val newStackHi = Stack.blank(sigCount = sigCountHi, promptCount = promptCountHi, frameCount = frameCountHi, headFeatures = featuresHi)
    val newStackLo = Stack.blank(sigCount = sigCountLo, promptCount = promptCountLo, frameCount = frameCountLo, headFeatures = featuresLo)
    //@#@OPTY reuse `oldStore` if number of elements stays the same
    val newStoreHi = oldStore.blankClone(localCountHi)
    val newStoreLo = oldStore.blankClone(localCountLo)
    fill(splitsHi, newStackHi, newStoreHi)
    fill(splitsLo, newStackLo, newStoreLo)
    newStackHi.integrityCheck()
    newStackLo.integrityCheck()
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
    stepHi: Step,
    stackHi: Stack,
    storeHi: Store,
    stepLo: Step,
    //@#@OPTY fuse with `store.setIfNotVoid``
    // local: Local,
    // location: Location.Shallow,
    stackLo: Stack,
    storeLo: Store,
  ): (Step, Stack, Store) =
    if stackHi.nonEmptySegment then
      val (stack, store) = reverse(
        todoStack = stackHi,
        todoStore = storeHi,
        accumStack = stackLo,
        accumStore = storeLo,
        aside = stepLo,
      )
      (stepHi, stack, store)
    else
      if stackHi.isTailless then
        //// Continuation's head segment is empty and has no tail
        val step2 = stepHi.append(stepLo)
        (step2, stackLo, storeLo)
      else
        //// Continuation's head segment is empty, but it does have tail
        val (stack, store) = reverse(
          todoStack = stackHi.tail,
          todoStore = storeHi.tail,
          accumStack = stackLo,
          accumStore = storeLo,
          aside = stepLo,
        )
        (stepHi, stack, store)


  private def reverse(
    todoStack: Stack,
    todoStore: Store,
    accumStack: Stack,
    accumStore: Store,
    aside: Step, //// Replaces null of continuation's head segment's `aside`
  ): (Stack, Store) =
    val newStack = todoStack.copyWithTail2(accumStack, aside)
    val newStore = todoStore.copyWithTail(accumStore)
    if todoStack.isTailless then
      (newStack, newStore)
    else
      reverseLoop(
        todoStack = todoStack.tail,
        todoStore = todoStore.tail,
        accumStack = newStack,
        accumStore = newStore,
      )


  @tailrec private def reverseLoop(
    todoStack: Stack,
    todoStore: Store,
    accumStack: Stack,
    accumStore: Store,
  ): (Stack, Store) =
    val newStack = todoStack.copyWithTail(accumStack)
    val newStore = todoStore.copyWithTail(accumStore)
    if todoStack.isTailless then
      (newStack, newStore)
    else
      reverseLoop(
        todoStack = todoStack.tail,
        todoStore = todoStore.tail,
        accumStack = newStack,
        accumStore = newStore,
      )

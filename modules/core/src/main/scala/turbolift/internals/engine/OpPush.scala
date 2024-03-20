package turbolift.internals.engine
import scala.annotation.tailrec


private[engine] object OpPush:
  def pushBase(stack: Stack, store: Store, step: Step, prompt: Prompt, stan: Stan): (Stack, Store) =
    if stack.head.size < Location.MAX_SEGMENT_SIZE then
      stack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
        store.deconsAndThen: (oldStoreSeg, moreStore) =>
          val newLocation = Location.Shallow(
            promptIndex = oldStackSeg.size,
            stanIndex = oldStoreSeg.size,
            isStateful = prompt.isStateful,
          )
          val newStackSeg = oldStackSeg.pushBase(newLocation, step, prompt)
          val newStoreSeg = if prompt.isStateful then oldStoreSeg.push(stan) else oldStoreSeg
          val newStack = newStackSeg ::? (moreStack, moreStep)
          val newStore = newStoreSeg ::? moreStore
          (newStack, newStore)
    else
      newTopSegment(stack, store, step, prompt, stan, isLocal = false, FrameKind.plain)


  def pushLocal(stack: Stack, store: Store, step: Step, prompt: Prompt, location: Location.Deep, stan: Stan, kind: FrameKind): (Stack, Store) =
    if location.segmentDepth == 0 then
      stack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
        store.deconsAndThen: (oldStoreSeg, moreStore) =>
          val loc = location.asShallow
          val oldStan = oldStoreSeg.getOrElseVoidSh(loc)
          val newStoreSeg = oldStoreSeg.setIfNotVoidSh(loc, stan)
          val newStackSeg = oldStackSeg.pushNextLocal(loc, step, oldStan, kind)
          val newStack = newStackSeg ::? (moreStack, moreStep)
          val newStore = newStoreSeg ::? moreStore
          (newStack, newStore)
    else
      newTopSegment(stack, store, step, prompt, stan, isLocal = true, kind)


  private def newTopSegment(stack: Stack, store: Store, step: Step, prompt: Prompt, stan: Stan, isLocal: Boolean, kind: FrameKind): (Stack, Store) =
    val newStackSeg = StackSegment.pushFirst(prompt, isLocal, kind)
    val newStoreSeg = StoreSegment.pushFirst(stan)
    val newStack = newStackSeg ::? (stack, step)
    val newStore = newStoreSeg ::? store
    (newStack, newStore)


  def drop(stack: Stack, store: Store): (Stack, Store, Step) =
    stack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
      store.deconsAndThen: (oldStoreSeg, moreStore) =>
        assert(oldStackSeg.isEmpty)
        assert(oldStoreSeg.isEmpty)
        (moreStack.nn, moreStore.nn, moreStep.nn)


  def pop(stack: Stack, store: Store): (Stack, Store, Step, Prompt, Frame, Stan) =
    stack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
      store.deconsAndThen: (oldStoreSeg, moreStore) =>
        if oldStackSeg.frameCount == 1 then
          //// Fast path: this is the last frame in top segment, so pop entire segment
          val topFrame = oldStackSeg.piles.head.topFrame
          val topPrompt = oldStackSeg.prompts.head
          val lastStan = if topPrompt.isStateless then Stan.void else oldStoreSeg.head
          (moreStack.nn, moreStore.nn, moreStep.nn, topPrompt, topFrame, lastStan)
        else
          //// Slow path: shrink top segment by 1 pile
          val location = oldStackSeg.locateHighestPile
          val topFrame = oldStackSeg.piles(location.promptIndex).topFrame
          val topPrompt = oldStackSeg.prompts(location.promptIndex)
          val newStackSeg =
            if topFrame.hasNext then
              oldStackSeg.popNextLocal(location)
            else
              oldStackSeg.popLast(topPrompt)
          //// restore saved state, but return the current one
          val (newStoreSeg, lastStan) = 
            if topPrompt.isStateless then
              (oldStoreSeg, Stan.void)
            else
              val lastStan = oldStoreSeg.getSh(location)
              val newStoreSeg =
                if topFrame.hasNext then
                  oldStoreSeg.setSh(location, topFrame.stan)
                else
                  oldStoreSeg.pop
              (newStoreSeg, lastStan)
          val newStack = newStackSeg ::? (moreStack, moreStep)
          val newStore = newStoreSeg ::? moreStore
          (newStack, newStore, topFrame.step, topPrompt, topFrame, lastStan)

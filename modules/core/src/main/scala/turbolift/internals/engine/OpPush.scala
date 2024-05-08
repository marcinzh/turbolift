package turbolift.internals.engine
import scala.annotation.tailrec


private[engine] object OpPush:
  def pushBase(stack: Stack, store: Store, step: Step, prompt: Prompt, local: Local): (Stack, Store) =
    if stack.head.size < Location.MAX_SEGMENT_SIZE then
      stack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
        store.deconsAndThen: (oldStoreSeg, moreStore) =>
          val newLocation = Location.Shallow(
            promptIndex = oldStackSeg.size,
            localIndex = oldStoreSeg.size,
            isStateful = prompt.isStateful,
          )
          val newStackSeg = oldStackSeg.pushBase(newLocation, step, prompt)
          val newStoreSeg = if prompt.isStateful then oldStoreSeg.push(local) else oldStoreSeg
          val newStack = newStackSeg ::? (moreStack, moreStep)
          val newStore = newStoreSeg ::? moreStore
          (newStack, newStore)
    else
      newTopSegment(stack, store, step, prompt, local, isNested = false, FrameKind.plain)


  def pushNested(stack: Stack, store: Store, step: Step, prompt: Prompt, location: Location.Deep, local: Local, kind: FrameKind): (Stack, Store) =
    if location.segmentDepth == 0 then
      stack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
        store.deconsAndThen: (oldStoreSeg, moreStore) =>
          val loc = location.asShallow
          val oldLocal = oldStoreSeg.getOrElseVoidSh(loc)
          val newStoreSeg = oldStoreSeg.setIfNotVoidSh(loc, local)
          val newStackSeg = oldStackSeg.pushNextNested(loc, step, oldLocal, kind)
          val newStack = newStackSeg ::? (moreStack, moreStep)
          val newStore = newStoreSeg ::? moreStore
          (newStack, newStore)
    else
      newTopSegment(stack, store, step, prompt, local, isNested = true, kind)


  private def newTopSegment(stack: Stack, store: Store, step: Step, prompt: Prompt, local: Local, isNested: Boolean, kind: FrameKind): (Stack, Store) =
    val newStackSeg = StackSegment.pushFirst(prompt, isNested, kind)
    val newStoreSeg = StoreSegment.pushFirst(local)
    val newStack = newStackSeg ::? (stack, step)
    val newStore = newStoreSeg ::? store
    (newStack, newStore)


  def drop(stack: Stack, store: Store): (Stack, Store, Step) =
    stack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
      store.deconsAndThen: (oldStoreSeg, moreStore) =>
        assert(oldStackSeg.isEmpty)
        assert(oldStoreSeg.isEmpty)
        (moreStack.nn, moreStore.nn, moreStep.nn)


  def pop(stack: Stack, store: Store): (Stack, Store, Step, Prompt, Frame, Local) =
    stack.deconsAndThen: (oldStackSeg, moreStack, moreStep) =>
      store.deconsAndThen: (oldStoreSeg, moreStore) =>
        if oldStackSeg.frameCount == 1 then
          //// Fast path: this is the last frame in top segment, so pop entire segment
          val topFrame = oldStackSeg.piles.head.topFrame
          val topPrompt = oldStackSeg.prompts.head
          val lastLocal = if topPrompt.isStateless then Local.void else oldStoreSeg.head
          (moreStack.nn, moreStore.nn, moreStep.nn, topPrompt, topFrame, lastLocal)
        else
          //// Slow path: shrink top segment by 1 pile
          val location = oldStackSeg.locateHighestPile
          val topFrame = oldStackSeg.piles(location.promptIndex).topFrame
          val topPrompt = oldStackSeg.prompts(location.promptIndex)
          val newStackSeg =
            if topFrame.hasNext then
              oldStackSeg.popNextNested(location)
            else
              oldStackSeg.popLast(topPrompt)
          //// restore saved state, but return the current one
          val (newStoreSeg, lastLocal) = 
            if topPrompt.isStateless then
              (oldStoreSeg, Local.void)
            else
              val lastLocal = oldStoreSeg.getSh(location)
              val newStoreSeg =
                if topFrame.hasNext then
                  oldStoreSeg.setSh(location, topFrame.local)
                else
                  oldStoreSeg.pop
              (newStoreSeg, lastLocal)
          val newStack = newStackSeg ::? (moreStack, moreStep)
          val newStore = newStoreSeg ::? moreStore
          (newStack, newStore, topFrame.step, topPrompt, topFrame, lastLocal)

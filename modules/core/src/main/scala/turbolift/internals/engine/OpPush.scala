package turbolift.internals.engine
import scala.annotation.tailrec


private object OpPush:
  def findTopmostEnv(stack: Stack, store: Store): Env =
    store.getDeep(stack.locateIO).asEnv


  def pushBase(stack: Stack, store: Store, step: Step, prompt: Prompt, local: Local): (Stack, Store) =
    if stack.promptCount <= Location.MAX_SEGMENT_SIZE then
      val newStack = stack.pushBase(prompt, step, localIndex = store.nextStoreIndex)
      val newStore = if prompt.isStateful then store.push(local) else store
      (newStack, newStore)
    else
      newTopSegment(stack, store, step, prompt, local, isNested = false, FrameKind.plain)


  def pushNested(stack: Stack, store: Store, step: Step, prompt: Prompt, location: Location.Deep, local: Local, kind: FrameKind): (Stack, Store) =
    if location.segmentDepth == 0 then
      val loc = location.shallow
      if prompt.isStateful then
        val oldLocal = store.getShallow(loc)
        val newStore = if local.isVoid then store else store.setShallow(loc, local)
        val newStack = stack.pushNested(promptIndex = loc.promptIndex, step, oldLocal, kind)
        (newStack, newStore)
      else
        val newStack = stack.pushNested(promptIndex = loc.promptIndex, step, Local.void, kind)
        (newStack, store)
    else
      newTopSegment(stack, store, step, prompt, local, isNested = true, kind)


  def pushNestedIO(stack: Stack, store: Store, step: Step, local: Local, kind: FrameKind): (Stack, Store) =
    val location = stack.locateIO
    pushNested(stack, store, step, PromptIO, location, local, kind)


  private def newTopSegment(stack: Stack, store: Store, step: Step, prompt: Prompt, local: Local, isNested: Boolean, kind: FrameKind): (Stack, Store) =
    val newStack = stack.pushNewSegment(step, prompt, isNested, kind)
    val newStore = store.pushNewSegment(local)
    (newStack, newStore)


  def drop(stack: Stack, store: Store): (Stack, Store, Step) =
    (stack.tail, store.tail, stack.aside.nn)


  def pop(stack: Stack, store: Store): (Stack, Store, Step, Prompt, Frame, Local) =
    if stack.frameCount == 1 then
      //// Fast path: this is the last frame in top segment, so pop entire segment
      val topPile = stack.piles.head
      val topPrompt = topPile.prompt
      val lastLocal = if topPrompt.isStateless then Local.void else store.head
      (stack.tail, store.tail, stack.aside.nn, topPrompt, topPile.topFrame, lastLocal)
    else
      //// Slow path: shrink top segment by 1 pile
      val location = stack.locateHighestPile
      val topPile = stack.piles(location.promptIndex)
      val topFrame = topPile.topFrame
      val topPrompt = topPile.prompt
      val newStack =
        if topFrame.hasNext then
          stack.popNested(location.promptIndex)
        else
          stack.popLast(topPrompt)
      //// restore saved state, but return the current one
      val (newStore, lastLocal) = 
        if topPrompt.isStateless then
          (store, Local.void)
        else
          val lastLocal = store.getShallow(location)
          val newStore =
            if topFrame.hasNext then
              store.setShallow(location, topFrame.local)
            else
              store.pop
          (newStore, lastLocal)
      (newStack, newStore, topFrame.step, topPrompt, topFrame, lastLocal)

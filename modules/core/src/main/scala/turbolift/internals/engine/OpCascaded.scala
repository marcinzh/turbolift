package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.!!


private[engine] object OpCascaded:
  def restart(stack: Stack, ftor: Any): AnyComp =
    def loop(todo: Stack): AnyComp =
      todo.deconsAndThen: (seg, more, _) =>
        val comp = 
          if more == null then
            !!.pure(ftor)
          else
            loop(more)
        restartSegment(seg, comp)
    loop(stack)


  def zip(stack: Stack, ftorLeft: Any, ftorRight: Any, fun: (Any, Any) => Any): Any =
    def loop(todo: Stack, a: Any, b: Any, f: (Any, Any) => Any): Any =
      todo.deconsAndThen: (seg, more, _) =>
        if more == null then
          zipSegment(seg, a, b, f)
        else
          loop(more, a, b, (x, y) => zipSegment(seg, x, y, f))
    loop(stack, ftorLeft, ftorRight, fun)


  def fork(stack: Stack, store: Store): (Store, Store) =
    def loop(todoStack: Stack, todoStore: Store): (Store, Store) =
      todoStack.deconsAndThen: (stackSeg, moreStack, _) =>
        todoStore.deconsAndThen: (oldStoreSeg, moreStore) =>
          val pairOfSegs = forkSegment(stackSeg, oldStoreSeg)
          if moreStack != null then
            val (newStoreTail1, newStoreTail2) = loop(moreStack, moreStore.nn)
            val (newStoreSeg1, newStoreSeg2) = pairOfSegs
            val newStore1 = newStoreSeg1 ::! newStoreTail1
            val newStore2 = newStoreSeg2 ::! newStoreTail2
            (newStore1, newStore2)
          else
            pairOfSegs.asPairOfStores
    loop(stack, store)


  def join(stack: Stack, store: Store, storeLeft: Store, storeRight: Store): Store =
    //@#@TODO unused due to disabled opty
    ???


  def zipAndRestart(stack: Stack, ftorLeft: Any, ftorRight: Any, fun: (Any, Any) => Any): AnyComp =
    val ftorOut = zip(stack, ftorLeft, ftorRight, fun)
    restart(stack, ftorOut)

  //------------------------------------------------------------
  // Segmentwise
  //------------------------------------------------------------

  private def restartSegment(seg: StackSegment, comp: AnyComp): AnyComp =
    val n = seg.prompts.size
    @tailrec def loop(i: Int, accum: AnyComp): AnyComp =
      if i < n then
        val p = seg.prompts(i)
        val i2 = i + 1
        if p.hasRestart then
          loop(i2, accum.flatMap(p.interpreter.onRestart))
        else
          loop(i2, accum)
      else
        accum
    loop(0, comp)


  private def zipSegment(seg: StackSegment, ftorLeft: Any, ftorRight: Any, fun: (Any, Any) => Any): Any =
    val n = seg.prompts.size
    def loop(i: Int, aa: Any, bb: Any, f: (Any, Any) => Any): Any =
      if i < n then
        val p = seg.prompts(i)
        val i2 = i + 1
        if p.hasZip then
          p.interpreter.onZip(aa, bb, loop(i2, _, _, f))
        else
          loop(i2, aa, bb, f)
      else
        f(aa, bb)
    loop(0, ftorLeft, ftorRight, fun)


  private def forkSegment(stackSeg: StackSegment, storeSeg: StoreSegment): (StoreSegment, StoreSegment) =
    if storeSeg.isEmpty then
      StoreSegment.emptyPair
    else
      val storeSegLeft = storeSeg.blankClone()
      val storeSegRight = storeSeg.blankClone()
      val n = stackSeg.prompts.size
      @tailrec def loop(i: Int, j: Int): Unit =
        if i < n then
          val p = stackSeg.prompts(i)
          if p.isStateful then
            val s0 = storeSeg.geti(j)
            if p.hasForkJoin then
              val (s1, s2) = p.interpreter.onFork(s0)
              storeSegLeft.setInPlace(j, s1.asLocal)
              storeSegRight.setInPlace(j, s2.asLocal)
            else
              storeSegLeft.setInPlace(j, s0)
              storeSegRight.setInPlace(j, s0)
            loop(i + 1, j + 1)
          else
            loop(i + 1, j)
      loop(0, 0)
      (storeSegLeft, storeSegRight)


  private def joinSegment(stackSeg: StackSegment, storeSeg: StoreSegment, storeSegLeft: StoreSegment, storeSegRight: StoreSegment): StoreSegment =
    if storeSeg.isEmpty then
      storeSeg
    else
      val storeSegOut = storeSeg.blankClone()
      val n = stackSeg.prompts.size
      @tailrec def loop(i: Int, j: Int): Unit =
        if i < n then
          val p = stackSeg.prompts(i)
          if p.isStateful then
            val s0 = storeSeg.geti(j)
            if p.hasForkJoin then
              val s1 = storeSegLeft.geti(j)
              val s2 = storeSegRight.geti(j)
              val s01 = p.interpreter.onJoin(s0, s1)
              val s012 = p.interpreter.onJoin(s01, s2)
              storeSegOut.setInPlace(j, s012.asLocal)
            else
              storeSegOut.setInPlace(j, s0)
            loop(i + 1, j + 1)
          else
            loop(i + 1, j)
      loop(0, 0)
      storeSegOut

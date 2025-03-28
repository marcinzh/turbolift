package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.!!
import Local.Syntax._


private[engine] object OpCascaded:
  def restart(stack: Stack, ftor: Any): AnyComp =
    def loop(todo: Stack): AnyComp =
      val comp = 
        if todo.isTailless then
          !!.pure(ftor)
        else
          loop(todo.tail)
      restartSegment(todo, comp)
    loop(stack)


  def unknown(stack: Stack, ftor: Any): Option[Any] =
    def loop(todo: Stack): Option[Any] =
      if todo.isTailless then
        unknownSegment(todo, ftor)
      else
        loop(todo.tail).flatMap(unknownSegment(todo, _))
    loop(stack)


  def zip(stack: Stack, ftorLeft: Any, ftorRight: Any, fun: (Any, Any) => Any): Any =
    def loop(todo: Stack, a: Any, b: Any, f: (Any, Any) => Any): Any =
      if todo.isTailless then
        zipSegment(todo, a, b, f)
      else
        loop(todo.tail, a, b, (x, y) => zipSegment(todo, x, y, f))
    loop(stack, ftorLeft, ftorRight, fun)


  def fork1(fromStack: Stack, fromStore0: Store, toStack: Stack): (Store, Store) =
    val fromStore = fromStore0.deepClone()
    def loop(stackSeg: Stack): Store =
      val tail = if stackSeg.hasTail then loop(stackSeg.tail) else null
      val storeSeg = Store.blank(stackSeg.localCount)
      val n = stackSeg.promptCount
      var i = 0
      while i < n do
        val prompt = stackSeg.piles(i).prompt
        if prompt.isStateful then
          val entry = fromStack.findEntryByPrompt(prompt)
          val s0 = fromStore.deepGet(entry.storeIndex, entry.segmentDepth)
          if prompt.hasForkJoin then
            val (s1, s2) = prompt.onFork(s0)
            fromStore.deepPutInPlace(entry.storeIndex, entry.segmentDepth, s1.asLocal)
            storeSeg.setInPlace(i, s2.asLocal)
          else
            storeSeg.setInPlace(i, s0.asLocal)
        i += 1
      storeSeg.copyWithTail(tail)
    val toStore = loop(toStack)
    (fromStore, toStore)


  def fork2(fromStack: Stack, fromStore0: Store, toStack: Stack): (Store, Store, Store) =
    val fromStore = fromStore0.deepClone()
    def loop(stackSeg: Stack): (Store, Store) =
      val (tail1, tail2) = if stackSeg.hasTail then loop(stackSeg.tail) else (null, null)
      val storeSeg1 = Store.blank(stackSeg.localCount)
      val storeSeg2 = Store.blank(stackSeg.localCount)
      val n = stackSeg.promptCount
      var i = 0
      while i < n do
        val prompt = stackSeg.piles(i).prompt
        if prompt.isStateful then
          val entry = fromStack.findEntryByPrompt(prompt)
          val s0 = fromStore.deepGet(entry.storeIndex, entry.segmentDepth)
          if prompt.hasForkJoin then
            val (s1, s2) = prompt.onFork(s0)
            val (s3, s4) = prompt.onFork(s1)
            fromStore.deepPutInPlace(entry.storeIndex, entry.segmentDepth, s3.asLocal)
            storeSeg1.setInPlace(i, s2.asLocal)
            storeSeg2.setInPlace(i, s4.asLocal)
          else
            storeSeg1.setInPlace(i, s0.asLocal)
            storeSeg2.setInPlace(i, s0.asLocal)
        i += 1
      (storeSeg1.copyWithTail(tail1), storeSeg2.copyWithTail(tail2))
    val (toStore1, toStore2) = loop(toStack)
    (fromStore, toStore1, toStore2)


  def join(stack: Stack, store: Store, storeLeft: Store, storeRight: Store): Store =
    //@#@TODO unused due to disabled opty
    ???


  def zipAndRestart(stack: Stack, ftorLeft: Any, ftorRight: Any, fun: (Any, Any) => Any): AnyComp =
    val ftorOut = zip(stack, ftorLeft, ftorRight, fun)
    restart(stack, ftorOut)


  //------------------------------------------------------------
  // Segmentwise
  //------------------------------------------------------------


  private def restartSegment(seg: Stack, comp: AnyComp): AnyComp =
    val n = seg.promptCount
    @tailrec def loop(i: Int, accum: AnyComp): AnyComp =
      if i < n then
        val p = seg.piles(i).prompt
        val i2 = i + 1
        if p.hasRestart then
          loop(i2, accum.flatMap(p.onRestart))
        else
          loop(i2, accum)
      else
        accum
    loop(0, comp)


  private def unknownSegment(seg: Stack, ftor: Any): Option[Any] =
    val n = seg.promptCount
    @tailrec def loop(i: Int, accum: Any): Option[Any] =
      if i < n then
        val p = seg.piles(i).prompt
        val i2 = i + 1
        if p.hasRestart then
          p.onUnknown(accum) match
            case Some(ftor2) => loop(i2, ftor2)
            case None => None
        else
          loop(i2, accum)
      else
        Some(accum)
    loop(0, ftor)


  private def zipSegment(seg: Stack, ftorLeft: Any, ftorRight: Any, fun: (Any, Any) => Any): Any =
    val n = seg.promptCount
    def loop(i: Int, aa: Any, bb: Any, f: (Any, Any) => Any): Any =
      if i < n then
        val p = seg.piles(i).prompt
        val i2 = i + 1
        if p.hasZip then
          p.onZip(aa, bb, loop(i2, _, _, f))
        else
          loop(i2, aa, bb, f)
      else
        f(aa, bb)
    loop(0, ftorLeft, ftorRight, fun)


  private def joinSegment(stack: Stack, store: Store, storeLeft: Store, storeRight: Store): Store =
    if store.isEmpty then
      store
    else
      val storeOut = store.blankClone()
      val n = stack.promptCount
      @tailrec def loop(i: Int, j: Int): Unit =
        if i < n then
          val p = stack.piles(i).prompt
          if p.isStateful then
            val s0 = store.geti(j)
            if p.hasForkJoin then
              val s1 = storeLeft.geti(j)
              val s2 = storeRight.geti(j)
              val s01 = p.onJoin(s0, s1)
              val s012 = p.onJoin(s01, s2)
              storeOut.setInPlace(j, s012.asLocal)
            else
              storeOut.setInPlace(j, s0)
            loop(i + 1, j + 1)
          else
            loop(i + 1, j)
      loop(0, 0)
      storeOut

package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.!!
import turbolift.internals.engine.Misc.AnyComp
import Prompt.Syntax._
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


  def zip(stack: Stack, ftorLeft: Any, ftorRight: Any, fun: (Any, Any) => Any): Any =
    def loop(todo: Stack, a: Any, b: Any, f: (Any, Any) => Any): Any =
      if todo.isTailless then
        zipSegment(todo, a, b, f)
      else
        loop(todo.tail, a, b, (x, y) => zipSegment(todo, x, y, f))
    loop(stack, ftorLeft, ftorRight, fun)


  def fork(stack: Stack, store: Store): (Store, Store) =
    def loop(todoStack: Stack, todoStore: Store): (Store, Store) =
      if !todoStack.accumFeatures.hasForkJoin then
        (todoStore, todoStore)
      else
        val pairOfSegs = forkSegment(todoStack, todoStore)
        if !todoStack.isTailless then
          val (newStoreTail1, newStoreTail2) = loop(todoStack.tail, todoStore.tail)
          val (newStoreHead1, newStoreHead2) = pairOfSegs
          //@#@OPTY {{ eliminate 1 needless copy, consider `head.appendInPlace(tail)`
          val newStore1 = newStoreHead1 ::? newStoreTail1
          val newStore2 = newStoreHead2 ::? newStoreTail2
          //@#@OPTY }}
          (newStore1, newStore2)
        else
          pairOfSegs
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


  private def forkSegment(stack: Stack, store: Store): (Store, Store) =
    if store.isEmpty then
      (store, store)
    else
      val storeLeft = store.blankClone()
      val storeRight = store.blankClone()
      val n = stack.promptCount
      @tailrec def loop(i: Int, j: Int): Unit =
        if i < n then
          val p = stack.piles(i).prompt
          if p.isStateful then
            val s0 = store.geti(j)
            if p.hasForkJoin then
              val (s1, s2) = p.onFork(s0)
              storeLeft.setInPlace(j, s1.asLocal)
              storeRight.setInPlace(j, s2.asLocal)
            else
              storeLeft.setInPlace(j, s0)
              storeRight.setInPlace(j, s0)
            loop(i + 1, j + 1)
          else
            loop(i + 1, j)
      loop(0, 0)
      (storeLeft, storeRight)


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

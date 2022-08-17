package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.!!


extension (thiz: Stack)
  private[engine] def cascadedPure(value: Any, store: Store): Any =
    @tailrec def loop(i: Int, aa: Any): Any =
      if i >= 0 then
        val p = thiz.promptAt(i)
        val s = store.getOrElseVoid(p)
        val bb = p.flow.onPure(aa, s)
        loop(i - 1, bb)
      else
        aa
    loop(thiz.promptCount - 1, value)

  private[engine] def cascadedUnpure(ftor: Any): AnyComp =
    val n = thiz.promptCount
    @tailrec def loop(i: Int, comp: AnyComp): AnyComp =
      if i < n then
        val p = thiz.promptAt(i)
        val i2 = i + 1
        if p.flow.hasUnpure then
          loop(i2, comp.flatMap(p.flow.onUnpure))
        else
          loop(i2, comp)
      else
        comp
    loop(0, !!.pure(ftor))

  private[engine] def cascadedZipWith(ftorLeft: Any, ftorRight: Any, fun: (Any, Any) => Any): Any =
    val n = thiz.promptCount
    def loop(i: Int, aa: Any, bb: Any, f: (Any, Any) => Any): Any =
      if i < n then
        val p = thiz.promptAt(i)
        val i2 = i + 1
        if p.flow.hasZip then
          p.flow.onZip(aa, bb, loop(i2, _, _, f))
        else
          loop(i2, aa, bb, f)
      else
        f(aa, bb)
    loop(0, ftorLeft, ftorRight, fun)

  private[engine] def cascadedFork(store: Store): (Store, Store) =
    if store.isEmptyOMG then
      Store.emptyPair
    else
      val ss1 = store.clone
      val ss2 = store.clone
      val n = thiz.promptCount
      @tailrec def loop(i: Int): Unit =
        if i < n then
          val p = thiz.promptAt(i)
          if p.flow.hasForkJoin then
            val j = p.storeIndex
            val s = store.get(j)
            val (s1, s2) = p.flow.onFork(s)
            ss1.setInPlace(j, s1)
            ss2.setInPlace(j, s2)
          loop(i + 1)
      loop(0)
      (ss1, ss2)

  //@#@ unused due to disabled opty
  private[engine] def cascadedJoin(store: Store, storeLeft: Store, storeRight: Store): Store =
    if store.isEmptyOMG then
      store
    else
      val ss = store.clone
      val n = thiz.promptCount
      @tailrec def loop(i: Int): Unit =
        if i < n then
          val p = thiz.promptAt(i)
          if p.flow.hasForkJoin then
            val j = p.storeIndex
            val s0 = store.get(j)
            val s1 = storeLeft.get(j)
            val s2 = storeRight.get(j)
            val s01 = p.flow.onJoin(s0, s1)
            val s012 = p.flow.onJoin(s01, s2)
            ss.setInPlace(j, s012)
          loop(i + 1)
      loop(0)
      ss

  private[engine] def cascadedZipAndUnpure(ftorLeft: Any, ftorRight: Any, fun: (Any, Any) => Any): AnyComp =
    val ftor = cascadedZipWith(ftorLeft, ftorRight, fun)
    cascadedUnpure(ftor)

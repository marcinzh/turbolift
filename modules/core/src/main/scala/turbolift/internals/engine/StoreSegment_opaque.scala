package turbolift.internals.engine


private[engine] trait StoreSegment_opaque:
  final def initial(env: Env): StoreSegment = StoreSegment.wrap(Array[Stan](env.asStan))
  final def blank(size: Int): StoreSegment = StoreSegment.wrap(new Array[Stan](size))

  final def pushFirst(s: Stan): StoreSegment = if s.isVoid then empty else StoreSegment.wrap(Array(s))
  final val empty: StoreSegment = StoreSegment.wrap(Array())
  final val emptyPair: (StoreSegment, StoreSegment) = (empty, empty)

  extension (thiz: StoreSegment)
    final def asStore: Store = Store.wrap(thiz)
    final def isEmpty: Boolean = thiz.unwrap.isEmpty
    final def size: Int = thiz.unwrap.size

    final def geti(i: Int): Stan = thiz.unwrap(i)
    final def seti(i: Int, s: Stan): StoreSegment = StoreSegment.wrap(thiz.unwrap.updated(i, s))
    final def stanCount: Int = thiz.unwrap.size
    final def setInPlace(i: Int, s: Stan): Unit = thiz.unwrap(i) = s

    final def getSh(l: Location.Shallow): Stan = geti(l.stanIndex)
    final def setSh(l: Location.Shallow, s: Stan): StoreSegment = seti(l.stanIndex, s)

    final def getOrElseVoidSh(l: Location.Shallow): Stan =
      if l.isStateless then Stan.void else geti(l.stanIndex)

    final def setIfNotVoidSh(l: Location.Shallow, s: Stan): StoreSegment =
      if s.isVoid then thiz else seti(l.stanIndex, s)

    final def push(s: Stan): StoreSegment = StoreSegment.wrap(thiz.unwrap :+ s)
    final def pop: StoreSegment = StoreSegment.wrap(thiz.unwrap.init)
    final def top: Stan = thiz.unwrap.last
    final def head: Stan = thiz.unwrap.head

    final def ::!(tail: Store): Store = StoreNel(thiz, tail.unwrap).asStore
    final def ::?(tail: Store | Null): Store =
      if tail == null then
        thiz.asStore
      else
        ::!(tail)

    final def blankClone(): StoreSegment = StoreSegment.blank(thiz.unwrap.size)

    final def toStr: String = thiz.unwrap.mkString("[", ", ", "]")

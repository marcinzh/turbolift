package turbolift.internals.engine


private trait StoreSegment_opaque:
  final def initial(env: Env): StoreSegment = StoreSegment.wrap(Array[Local](env.asLocal))
  final def blank(size: Int): StoreSegment = StoreSegment.wrap(new Array[Local](size))

  final def pushFirst(s: Local): StoreSegment = if s.isVoid then empty else StoreSegment.wrap(Array(s))
  final val empty: StoreSegment = StoreSegment.wrap(Array())
  final val emptyPair: (StoreSegment, StoreSegment) = (empty, empty)

  extension (thiz: StoreSegment)
    final def asStore: Store = Store.wrap(thiz)
    final def isEmpty: Boolean = thiz.unwrap.isEmpty
    final def size: Int = thiz.unwrap.size

    final def geti(i: Int): Local = thiz.unwrap(i)
    final def seti(i: Int, s: Local): StoreSegment = StoreSegment.wrap(thiz.unwrap.updated(i, s))
    final def localCount: Int = thiz.unwrap.size
    final def setInPlace(i: Int, s: Local): Unit = thiz.unwrap(i) = s

    final def getSh(l: Location.Shallow): Local = geti(l.localIndex)
    final def setSh(l: Location.Shallow, s: Local): StoreSegment = seti(l.localIndex, s)

    final def getOrElseVoidSh(l: Location.Shallow): Local =
      if l.isStateless then Local.void else geti(l.localIndex)

    final def setIfNotVoidSh(l: Location.Shallow, s: Local): StoreSegment =
      if s.isVoid then thiz else seti(l.localIndex, s)

    final def push(s: Local): StoreSegment = StoreSegment.wrap(thiz.unwrap :+ s)
    final def pop: StoreSegment = StoreSegment.wrap(thiz.unwrap.init)
    final def top: Local = thiz.unwrap.last
    final def head: Local = thiz.unwrap.head

    final def ::!(tail: Store): Store = StoreNel(thiz, tail.unwrap).asStore
    final def ::?(tail: Store | Null): Store =
      if tail == null then
        thiz.asStore
      else
        ::!(tail)

    final def blankClone(): StoreSegment = StoreSegment.blank(thiz.unwrap.size)

    final def toStr: String = thiz.unwrap.mkString("[", ", ", "]")

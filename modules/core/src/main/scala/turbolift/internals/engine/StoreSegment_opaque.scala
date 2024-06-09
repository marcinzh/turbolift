package turbolift.internals.engine


//// 0-th array member is a copy of the topmost Env

private trait StoreSegment_opaque:
  private inline val ENV_INDEX = 0
  private inline val RESERVED = 1
  //@#@TODO eliminate Nel
  // private inline val TAIL_INDEX = 0
  // private inline val ENV_INDEX = 1
  // private inline val RESERVED = 2

  final def initial(env: Env): StoreSegment = StoreSegment.wrap(Array[Local](env.asLocal, env.asLocal))

  final def pushFirst(env: Env, s: Local): StoreSegment =
    if s.isVoid then
      StoreSegment.wrap(Array(env.asLocal))
    else
      StoreSegment.wrap(Array(env.asLocal, s))

  extension (thiz: StoreSegment)
    final def asStore: Store = Store.wrap(thiz)
    final def isEmpty: Boolean = thiz.unwrap.size == RESERVED
    final def localCount: Int = thiz.unwrap.size - RESERVED
    final def newLocalIndex: Int = localCount

    final def geti(i: Int): Local = thiz.unwrap(i + RESERVED)
    final def seti(i: Int, s: Local): StoreSegment = StoreSegment.wrap(thiz.unwrap.updated(i + RESERVED, s))
    final def setInPlace(i: Int, s: Local): Unit = thiz.unwrap(i + RESERVED) = s

    final def getSh(l: Location.Shallow): Local = geti(l.localIndex)
    final def setSh(l: Location.Shallow, s: Local, isIo: Boolean): StoreSegment =
      val that = seti(l.localIndex, s)
      if isIo then
        that.setEnvAsLocalInPlace(s)
      that

    final def getOrElseVoidSh(l: Location.Shallow): Local =
      if l.isStateless then Local.void else geti(l.localIndex)

    final def setIfNotVoidSh(l: Location.Shallow, s: Local, isIo: Boolean): StoreSegment =
      if s.isVoid then
        thiz
      else
        val that = seti(l.localIndex, s)
        if isIo then
          that.setEnvAsLocalInPlace(s)
        that

    final def getEnv: Env = getEnvAsLocal.asInstanceOf[Env]
    final def getEnvAsLocal: Local = thiz.unwrap(ENV_INDEX)
    final def setEnv(env: Env): StoreSegment = StoreSegment.wrap(thiz.unwrap.updated(ENV_INDEX, env.asLocal))
    final def setEnvAsLocal(local: Local): StoreSegment = StoreSegment.wrap(thiz.unwrap.updated(ENV_INDEX, local))
    final def setEnvInPlace(env: Env): Unit = setEnvAsLocalInPlace(env.asLocal)
    final def setEnvAsLocalInPlace(local: Local): Unit = thiz.unwrap(ENV_INDEX) = local

    final def push(s: Local): StoreSegment = StoreSegment.wrap(thiz.unwrap :+ s)
    final def pop: StoreSegment = StoreSegment.wrap(thiz.unwrap.init)
    final def top: Local = thiz.unwrap.last
    final def head: Local = thiz.unwrap(1)

    final def ::!(tail: Store): Store = StoreNel(thiz, tail.unwrap).asStore
    final def ::?(tail: Store | Null): Store =
      if tail == null then
        thiz.asStore
      else
        ::!(tail)

    final def blankClone(): StoreSegment = blankClone(thiz.localCount)

    final def blankClone(newLocalCount: Int): StoreSegment =
      val arr1 = thiz.unwrap
      val arr2 = new Array[Local](newLocalCount + RESERVED)
      //@#@ TODO eliminate Nel
      // arr2(TAIL_INDEX) = arr1(TAIL_INDEX)
      arr2(ENV_INDEX) = arr1(ENV_INDEX)
      StoreSegment.wrap(arr2)

    final def toStr: String = thiz.unwrap.mkString("[", ", ", "]")

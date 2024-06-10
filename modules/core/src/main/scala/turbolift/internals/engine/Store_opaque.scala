package turbolift.internals.engine
import scala.annotation.tailrec


//// 0th array member is the tail
//// 1st array member is a copy of the topmost Env

private trait Store_opaque:
  private final inline val TAIL_INDEX = 0
  private final inline val ENV_INDEX = 1
  private final inline val RESERVED = 2

  final def initial(env: Env): Store = pushFirst(null, env, env.asLocal)

  final def pushFirst(tailOrNull : Store | Null, env: Env, s: Local): Store =
    if s.isVoid then
      Store.wrap(Array(tailOrNull.asLocal, env.asLocal))
    else
      Store.wrap(Array(tailOrNull.asLocal, env.asLocal, s))


  extension (thiz: Store)
    final def isEmpty: Boolean = thiz.unwrap.size == RESERVED
    final def localCount: Int = thiz.unwrap.size - RESERVED
    final def nextLocalIndex: Int = localCount


    final def getDeep(l: Location.Deep): Local =
      @tailrec def loop(todo: Store, depth: Int): Local =
        if depth == 0 then
          todo.geti(l.localIndex)
        else
          if !todo.isTailless then
            loop(todo.tail, depth - 1)
          else
            notFound(l)
      loop(thiz, l.segmentDepth)


    final def setDeep(l: Location.Deep, s: Local): Store =
      def loop(todo: Store, depth: Int): Store =
        if depth == 0 then
          todo.seti(l.localIndex, s)
        else
          if !todo.isTailless then
            todo ::? loop(todo.tail, depth - 1)
          else
            notFound(l)
      loop(thiz, l.segmentDepth)


    final def getDeepOrElseVoid(l: Location.Deep): Local =
      if l.isStateful then
        getDeep(l)
      else
        Local.void


    final def setDeepIfNotVoid(l: Location.Deep, s: Local): Store =
      if s.isVoid then
        thiz
      else
        setDeep(l, s)


    final def geti(i: Int): Local = thiz.unwrap(i + RESERVED)
    final def seti(i: Int, s: Local): Store = Store.wrap(thiz.unwrap.updated(i + RESERVED, s))
    final def setInPlace(i: Int, s: Local): Unit = thiz.unwrap(i + RESERVED) = s

    final def getSh(l: Location.Shallow): Local = geti(l.localIndex)
    final def setSh(l: Location.Shallow, s: Local, isIo: Boolean): Store =
      val that = seti(l.localIndex, s)
      if isIo then
        that.setEnvAsLocalInPlace(s)
      that

    final def getOrElseVoidSh(l: Location.Shallow): Local =
      if l.isStateless then Local.void else geti(l.localIndex)

    final def setIfNotVoidSh(l: Location.Shallow, s: Local, isIo: Boolean): Store =
      if s.isVoid then
        thiz
      else
        val that = seti(l.localIndex, s)
        if isIo then
          that.setEnvAsLocalInPlace(s)
        that

    final def getEnv: Env = getEnvAsLocal.asInstanceOf[Env]
    final def getEnvAsLocal: Local = thiz.unwrap(ENV_INDEX)
    final def setEnv(env: Env): Store = Store.wrap(thiz.unwrap.updated(ENV_INDEX, env.asLocal))
    final def setEnvAsLocal(local: Local): Store = Store.wrap(thiz.unwrap.updated(ENV_INDEX, local))
    final def setEnvInPlace(env: Env): Unit = setEnvAsLocalInPlace(env.asLocal)
    final def setEnvAsLocalInPlace(local: Local): Unit = thiz.unwrap(ENV_INDEX) = local

    final def push(s: Local): Store = Store.wrap(thiz.unwrap :+ s)
    final def pop: Store = Store.wrap(thiz.unwrap.init)
    final def top: Local = thiz.unwrap.last
    final def head: Local = thiz.unwrap(RESERVED)

    final def isTailless: Boolean = thiz.unwrap(TAIL_INDEX).asInstanceOf[Any] == null
    final def tail: Store = thiz.unwrap(TAIL_INDEX).asInstanceOf[Store]
    final def tailOrNull: Store | Null = thiz.unwrap(TAIL_INDEX).asInstanceOf[Store | Null]

    final def blankClone(): Store = blankClone(thiz.localCount)

    final def blankClone(newLocalCount: Int): Store =
      val arr1 = thiz.unwrap
      val arr2 = new Array[Local](newLocalCount + RESERVED)
      arr2(TAIL_INDEX) = arr1(TAIL_INDEX)
      arr2(ENV_INDEX) = arr1(ENV_INDEX)
      Store.wrap(arr2)

    final def blankClone(newLocalCount: Int, tailOrNull: Store | Null): Store =
      val arr1 = thiz.unwrap
      val arr2 = new Array[Local](newLocalCount + RESERVED)
      arr2(TAIL_INDEX) = tailOrNull.asLocal
      arr2(ENV_INDEX) = arr1(ENV_INDEX)
      Store.wrap(arr2)

    final def ::?(that: Store | Null): Store =
      Store.wrap(thiz.unwrap.updated(TAIL_INDEX, that.asLocal))

    //@#@TODO use
    final def setTailInPlace(tailOrNull: Store | Null): Unit =
      thiz.unwrap.updated(TAIL_INDEX, tailOrNull.asLocal)

    //@#@TODO use
    final def copyTailless: Store =
      val arr1 = thiz.unwrap
      val arr2 = new Array[Local](localCount + RESERVED)
      arr2(ENV_INDEX) = arr1(ENV_INDEX)
      val n = localCount
      var i = 0
      while i < n do
        arr2(i) = arr1(i)
        i += 1
      Store.wrap(arr2)

    final def toStr: String = s"Store(${toStrAux})"

    final def toStrAux: String =
      val a = thiz.unwrap.iterator.drop(RESERVED).mkString("[", ", ", "]")
      if isTailless then
        a
      else
        val b = tail.toStrAux
        s"$a | $b"


  private final def notFound(l: Location.Deep): Nothing = panic(s"Location ${l.toStr} not found")

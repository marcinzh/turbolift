package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.internals.engine.Env
import turbolift.internals.engine.Misc._
import Local.Syntax._


//// the last array member is the tail

private trait Store_opaque:
  private final inline val RESERVED = 1

  final def initial(env: Env): Store = Store.wrap(Array(env, null))

  extension (thiz: Store)
    final def isEmpty: Boolean = thiz.unwrap.size == RESERVED
    final def localCount: Int = thiz.unwrap.size - RESERVED
    final def nextStoreIndex: Int = localCount


    final def getDeep(l: Location.Deep): Local =
      @tailrec def loop(todo: Store, depth: Int): Local =
        if depth == 0 then
          todo.geti(l.storeIndex)
        else
          if !todo.isTailless then
            loop(todo.tail, depth - 1)
          else
            notFound(l)
      loop(thiz, l.segmentDepth)


    final def setDeep(l: Location.Deep, s: Local): Store =
      def loop(todo: Store, depth: Int): Store =
        if depth == 0 then
          todo.seti(l.storeIndex, s)
        else
          if !todo.isTailless then
            todo ::? loop(todo.tail, depth - 1)
          else
            notFound(l)
      loop(thiz, l.segmentDepth)


    final def setDeepIfNotVoid(l: Location.Deep, s: Local): Store =
      if s.isVoid then
        thiz
      else
        setDeep(l, s)


    final def geti(i: Int): Local = thiz.unwrap(i).asLocal
    final def seti(i: Int, s: Local): Store = Store.wrap(thiz.unwrap.updated(i, s))
    final def setInPlace(i: Int, s: Local): Unit = thiz.unwrap(i) = s
    final def getShallow(l: Location.Shallow): Local = geti(l.storeIndex)
    final def setShallow(l: Location.Shallow, s: Local): Store = seti(l.storeIndex, s)

    final def head: Local = thiz.unwrap(0).asLocal
    final def isTailless: Boolean = thiz.unwrap.last.asInstanceOf[Any] == null
    final def tail: Store = thiz.unwrap.last.asInstanceOf[Store]
    final def tailOrNull: Store | Null = thiz.unwrap.last.asInstanceOf[Store | Null]
    inline final def tailIndex: Int = thiz.unwrap.size - 1


    final def push(s: Local): Store =
      val arr = thiz.unwrap :+ thiz.tail
      arr(arr.size - 2) = s
      Store.wrap(arr)


    final def pop: Store =
      val t = thiz.unwrap.last
      val arr = thiz.unwrap.init
      arr(arr.size - 1) = t
      Store.wrap(arr)


    final def pushNewSegment(s: Local): Store =
      if s.isVoid then
        Store.wrap(Array(thiz))
      else
        Store.wrap(Array(s, thiz))


    final def blankClone(): Store = blankClone(thiz.localCount)


    final def blankClone(newLocalCount: Int): Store =
      val n = newLocalCount + RESERVED
      val arr = new Array[Any](n)
      arr(n - 1) = thiz.unwrap.last //// clone keeps the tail of the original
      Store.wrap(arr)


    final def ::?(that: Store | Null): Store =
      Store.wrap(thiz.unwrap.updated(thiz.tailIndex, that))


    final def setTailInPlace(tailOrNull: Store | Null): Unit =
      thiz.unwrap(thiz.tailIndex) = tailOrNull


    //@#@TODO use
    final def copyTailless: Store =
      val arr1 = thiz.unwrap
      val arr2 = new Array[Any](localCount + RESERVED)
      val n = localCount
      var i = 0
      while i < n do
        arr2(i) = arr1(i)
        i += 1
      Store.wrap(arr2)


    final def toStr: String = s"Store(${toStrAux})"

    final def toStrAux: String =
      val a = thiz.unwrap.iterator.take(localCount).mkString("[", ", ", "]")
      if isTailless then
        a
      else
        val b = tail.toStrAux
        s"$a | $b"


  private final def notFound(l: Location.Deep): Nothing = panic(s"Location ${l.toStr} not found")

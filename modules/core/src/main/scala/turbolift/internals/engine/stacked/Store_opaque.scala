package turbolift.internals.engine.stacked
import scala.annotation.tailrec
import turbolift.internals.engine.Env
import Local.Syntax._


//// the last array member is the tail

private trait Store_opaque:
  private final inline val RESERVED = 1

  final val empty: Store = Store.wrap(Array(null))

  final def initial(env: Env): Store = Store.wrap(Array(env, null))

  final def blank(localCount: Int): Store =
    val n = localCount + RESERVED
    val arr = new Array[Any](n)
    Store.wrap(arr)


  extension (thiz: Store)
    final def isEmpty: Boolean = thiz.unwrap.size == RESERVED
    final def localCount: Int = thiz.unwrap.size - RESERVED
    final def nextStoreIndex: Int = localCount


    @tailrec final def deepGet(storeIndex: Int, segmentDepth: Int): Local =
      if segmentDepth == 0 then
        geti(storeIndex)
      else
        //// YOLO, NPE if not found
        tail.deepGet(storeIndex, segmentDepth - 1)


    final def deepPut(storeIndex: Int, segmentDepth: Int, s: Local): Store =
      if segmentDepth == 0 then
        seti(storeIndex, s)
      else
        // YOLO, NPE if not found
        thiz.copyWithTail(tail.deepPut(storeIndex, segmentDepth - 1, s))


    final def deepClone(segmentDepth: Int): Store =
      if segmentDepth == 0 then
        thiz.copyWithTail(tail)
      else
        // YOLO, NPE if not found
        thiz.copyWithTail(tail.deepClone(segmentDepth - 1))


    @tailrec final def deepPutInPlace[A, S](storeIndex: Int, segmentDepth: Int, s: Local): Unit =
      if segmentDepth == 0 then
        setInPlace(storeIndex, s)
      else
        //// YOLO, NPE if not found
        tail.deepPutInPlace(storeIndex, segmentDepth - 1, s)


    @tailrec final def deepUpdateInPlace[A, S](storeIndex: Int, segmentDepth: Int, f: S => (A, S)): A =
      if segmentDepth == 0 then
        val s1 = geti(storeIndex)
        val a_s2 = f(s1.asInstanceOf[S])
        setInPlace(storeIndex, a_s2._2.asLocal)
        a_s2._1
      else
        //// YOLO, NPE if not found
        tail.deepUpdateInPlace(storeIndex, segmentDepth - 1, f)


    //@#@TODO update
    //// {{ old interface
    final def deepGet(l: Location.Deep): Local = deepGet(l.storeIndex, l.segmentDepth)

    final def deepPut(l: Location.Deep, s: Local): Store = deepPut(l.storeIndex, l.segmentDepth, s)

    final def deepPutIfNotVoid(l: Location.Deep, s: Local): Store =
      if s.isVoid then
        thiz
      else
        deepPut(l, s)
    //// }} old interface


    final inline def geti(i: Int): Local = thiz.unwrap(i).asLocal
    final inline def seti(i: Int, s: Local): Store =
      val arr = thiz.unwrap.clone()
      arr(i) = s
      Store.wrap(arr)
    final inline def setInPlace(i: Int, s: Local): Unit = thiz.unwrap(i) = s
    final inline def getShallow(l: Location.Shallow): Local = geti(l.storeIndex)
    final inline def setShallow(l: Location.Shallow, s: Local): Store = seti(l.storeIndex, s)

    final inline def head: Local = thiz.unwrap(0).asLocal
    final inline def isTailless: Boolean = thiz.unwrap.last.asInstanceOf[Any] == null
    final inline def tail: Store = thiz.unwrap.last.asInstanceOf[Store]
    final inline def tailOrNull: Store | Null = thiz.unwrap.last.asInstanceOf[Store | Null]
    final inline def tailIndex: Int = thiz.unwrap.size - 1


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


    final def deepClone(): Store =
      val arr = thiz.unwrap.clone()
      if !thiz.isTailless then
        arr(thiz.tailIndex) = thiz.tail.deepClone()
      Store.wrap(arr)


    final inline def copyWithTail(that: Store | Null): Store =
      val arr = thiz.unwrap.clone()
      arr(thiz.tailIndex) = that
      Store.wrap(arr)


    final inline def setTailInPlace(tailOrNull: Store | Null): Unit =
      thiz.unwrap(thiz.tailIndex) = tailOrNull


    //@#@TODO use
    final inline def copyTailless: Store =
      val arr = thiz.unwrap.clone()
      arr(thiz.tailIndex) = null
      Store.wrap(arr)


    final def toStr: String = s"Store(${toStrAux})"

    final def toStrAux: String =
      val a = thiz.unwrap.iterator.take(localCount).mkString("[", ", ", "]")
      if isTailless then
        a
      else
        val b = tail.toStrAux
        s"$a | $b"


  private final def notFound(l: Location.Deep): Nothing = panic(s"Location ${l.toStr} not found")

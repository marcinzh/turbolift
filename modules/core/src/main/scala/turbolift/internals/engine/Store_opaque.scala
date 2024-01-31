package turbolift.internals.engine
import scala.annotation.tailrec


private[engine] trait Store_opaque:
  final def initial(env: Env): Store = StoreSegment.initial(env).asStore

  private final def notFound(l: Location.Deep): Nothing = panic(s"Location ${l.toStr} not found")


  extension (thiz: Store)
    inline final def deconsAndThen[T](inline cb: (StoreSegment, Store | Null) => T): T =
      if thiz.isInstanceOf[Array[?]] then
        cb(thiz.asInstanceOf[StoreSegment], null)
      else
        val nel = thiz.asInstanceOf[StoreNel]
        cb(nel.head, nel.tail)


    final def get(l: Location.Deep): Stan =
      @tailrec def loop(todo: Store, depth: Int): Stan =
        todo.deconsAndThen: (seg, more) =>
          if depth == 0 then
            seg.geti(l.stanIndex)
          else
            if more != null then
              loop(more, depth - 1)
            else
              notFound(l)
      loop(thiz, l.segmentDepth)


    final def set(l: Location.Deep, s: Stan): Store =
      def loop(todo: Store, depth: Int): Store =
        todo.deconsAndThen: (seg, more) =>
          if depth == 0 then
            val seg2 = seg.seti(l.stanIndex, s)
            seg2 ::? more
          else
            if more != null then
              val more2 = loop(more, depth - 1)
              seg ::? more2
            else
              notFound(l)
      loop(thiz, l.segmentDepth)


    final def getOrElseVoid(l: Location.Deep): Stan =
      if l.isStateful then
        get(l)
      else
        Stan.void

    
    final def setIfNotVoid(l: Location.Deep, s: Stan): Store =
      if s.isVoid then
        thiz
      else
        set(l, s)


    final def toStr: String = s"Store(${toStrAux})"

    final def toStrAux: String =
      thiz.deconsAndThen: (head, tail) =>
        val a = head.toStr
        if tail == null then
          a
        else
          val b = tail.nn.toStrAux
          s"$a | $b"

package turbolift.internals.engine
import turbolift.internals.interpreter.Void


private[engine] opaque type Store = Array[Any]


private[engine] object Store:
  val empty: Store = new Array[Any](0)
  val emptyPair: (Store, Store) = (empty, empty)
  
  def toStr(store: Store): String = store.mkString("[", ", ", "]")
  def toStrOrNull(store: Store | Null): String = if store == null then "null" else toStr(store)

  extension (thiz: Store)
    def isEmptyOMG: Boolean = thiz.isEmpty

    def nextIndex: Int = thiz.size

    def get(i: Int): Any = thiz(i)

    //@#@TODO move config from Stack to Store at 0-th element
    // def getConfig: Config = get(0).asInstanceOf[Config]
    // def setConfig(cfg: Config): Store = set(0, cfg)

    def get(p: Prompt): Any = thiz(p.storeIndex)

    def setInPlace(i: Int, s: Any): Unit =
      thiz(i) = s

    def set(i: Int, s: Any): Store =
      val that = clone
      that(i) = s
      that

    def set(p: Prompt, s: Any): Store =
      val that = clone
      that(p.storeIndex) = s
      that

    def setInPlace(p: Prompt, s: Any): Unit =
      thiz(p.storeIndex) = s

    // def setInPlaceIfNotVoid(p: Prompt, s: Any): Unit =
    //   if Void != s then
    //     assert(p.hasStan)
    //     thiz.setInPlace(p, s)

    def setInPlaceIfHasStan(p: Prompt, s: Any): Unit =
      if p.hasStan then
        thiz.setInPlace(p, s)

    def getOrElseVoid(p: Prompt): Any =
      if !p.hasStan then
        Void
      else
        get(p)
    
    def setIfNotVoid(p: Prompt, s: Any): Store =
      if Void == s then
        thiz
      else
        set(p, s)

    def clone: Store =
      val n = thiz.size
      val that = new Array[Any](n)
      java.lang.System.arraycopy(thiz, 0, that, 0, n)
      that

    def pushIfHasStan(p: Prompt, s: Any): Store =
      if !p.hasStan then
        thiz
      else
        thiz :+ s

    def dropIfHasStan(p: Prompt): Store =
      if !p.hasStan then
        thiz
      else
        thiz.init

    def blank: Store = new Array[Any](thiz.size)

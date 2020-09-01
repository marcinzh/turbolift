package turbolift.abstraction.internals.engine


private[engine] object Vmt {
  val empty: Array[AnyRef] = prealloc(0)
  
  def prealloc(n: Int): Array[AnyRef] = {
    new Array[AnyRef]((n + 1) * 2)
  }
  
  //// must be a separate step, because creating `isChoice` ties the knot
  def fill[K <: AnyRef, V <: AnyRef, X](
    arr: Array[AnyRef],
    items: Iterable[X],
    makeKey: X => K,
    makeValue: X => V,
    isChoice: V => Boolean,
  ): Unit = {
    val n = items.size
    var lastChoice:V = null.asInstanceOf[V]
    for ((x, i) <- items.iterator.zipWithIndex) {
      val k:K = makeKey(x)
      val v:V = makeValue(x)
      arr(i*2) = k
      arr(i*2+1) = v
      if (isChoice(v)) {
        lastChoice = v
      }
    }
    arr(n*2) = null
    arr(n*2+1) = lastChoice
  }

  def lookup(arr: Array[AnyRef], key: AnyRef): AnyRef = {
    def loop(idx: Int): AnyRef = {
      if (arr(idx) eq key)
        arr(idx+1)
      else
        loop(idx+2)
    }
    loop(0)
  }
}

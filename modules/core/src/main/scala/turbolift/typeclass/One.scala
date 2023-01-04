package turbolift.typeclass


trait One[W, W1]:
  def one(e: W1): W


object One extends OneInstances2:
  def apply[W, W1](using ev: One[W, W1]) = ev


private trait OneInstances1:
  given [W]: One[W, W] with
    override def one(a: W): W = a


private trait OneInstances2 extends OneInstances1:
  given [W, W1](using accum: Accum[W, W1]): One[W, W1] with
    override def one(a: W1): W = accum.one(a)

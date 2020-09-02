package turbolift.std_effects
import cats.Monoid
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.AccumZero
import turbolift.std_handlers.DefaultAccumulatorHandler


trait AccumulatorSig[U, E] {
  def tell(e: E): Unit !! U
  def clear[A](scope: A !! U): A !! U
}

trait Accumulator[E] extends Effect[AccumulatorSig[?, E]] { thiz =>
  final def tell(e: E): Unit !! this.type = embedFO(_.tell(e))
  final def clear[A, U](scope: A !! U): A !! U with this.type = embedHO[U](_.clear(scope))

  object handler {
    def apply[W](implicit W: AccumZero[E, W]): ThisHandler[(W, ?)] = DefaultAccumulatorHandler[E, W, thiz.type](thiz)
    def monoid(implicit ev: Monoid[E]) = apply[E]
    def vector = apply[Vector[E]]
    def list = apply[List[E]]
    def set = apply[Set[E]]
  }
}

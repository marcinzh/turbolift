package turbolift.std_effects
import cats.Monoid
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.AccumZero
import turbolift.std_handlers.DefaultAccumulatorHandler


trait AccumulatorSig[U, E] extends Signature[U] {
  def tell(e: E): Unit !! U
  def clear[A](scope: A !! U): A !! U
}

trait Accumulator[E] extends Effect[AccumulatorSig[?, E]] { thiz =>
  def tell(e: E): Unit !! this.type = encodeFO(_.tell(e))
  def clear[A, U](scope: A !! U): A !! U with this.type = encodeHO[U](_.clear(scope))

  object handler {
    def apply[W](implicit W: AccumZero[E, W]) = DefaultAccumulatorHandler[E, W, thiz.type](thiz).apply(W.zero)
    def monoid(implicit ev: Monoid[E]) = apply[E]
    def vector = apply[Vector[E]]
    def list = apply[List[E]]
    def set = apply[Set[E]]
  }
}

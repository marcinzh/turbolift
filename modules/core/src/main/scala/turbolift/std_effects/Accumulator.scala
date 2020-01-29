package turbolift.std_effects
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}


trait AccumulatorSig[P[_], E] extends Signature[P] {
  def tell(e: E): P[Unit]
}


trait Accumulator[E] extends Effect[AccumulatorSig[?[_], E]] { thiz =>
  def tell(e: E): Unit !! this.type = encodeFO(_.tell(e))

  object handler {
    def apply[W](implicit W: PlusOneZero[E, W]) = DefaultAccumulatorHandler[E, W, thiz.type](thiz).apply(W.zero)
    def monoid(implicit ev: Monoid[E]) = apply[E]
    def vector = apply[Vector[E]]
    def list = apply[List[E]]
    def set = apply[Set[E]]
  }
}


object DefaultAccumulatorHandler {
  def apply[E, W, Fx <: Accumulator[E]](fx: Fx)(implicit W: PlusOneZero[E, W]) = new fx.Unary[W, (W, ?)] {
    val theFunctor = FunctorInstances.pair[W]

    def commonOps[M[_]: MonadPar] = new CommonOps[M] {
      def lift[A](ma: M[A]): W => M[(W, A)] = w => ma.map((w, _))

      def flatMap[A, B](tma: W => M[(W, A)])(f: A => W => M[(W, B)]): W => M[(W, B)] =
        w0 => tma(w0).flatMap {
          case (w1, a) => f(a)(w1)
        }

      def zipPar[A, B](tma: W => M[(W, A)], tmb: W => M[(W, B)]): W => M[(W, (A, B))] =
        w0 => (tma(W.zero) *! tmb(W.zero)).map {
          case ((w1, a), (w2, b)) => ((w0 |+| w1) |+| w2, (a, b))
        }
    }

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with AccumulatorSig[P, E] {
      def tell(e: E): P[Unit] = liftOuter(w => pureInner((w |+ e, ())))
    }
  }.self
}

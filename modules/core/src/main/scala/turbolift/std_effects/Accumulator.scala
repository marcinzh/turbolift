package turbolift.std_effects
import cats.Monoid
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.{MonadPar, AccumZero}
import turbolift.abstraction.implicits.{AccumSyntax, MonadParSyntax}


trait AccumulatorSig[P[_], E] extends Signature[P] {
  def tell(e: E): P[Unit]
  def clear[A](scope: P[A]): P[A]
}


trait Accumulator[E] extends Effect[AccumulatorSig[?[_], E]] { thiz =>
  def tell(e: E): Unit !! this.type = encodeFO(_.tell(e))
  def clear[A, U](scope: A !! U): A !! U with this.type = encodeHO[U](run => _.clear(run(scope)))

  object handler {
    def apply[W](implicit W: AccumZero[E, W]) = DefaultAccumulatorHandler[E, W, thiz.type](thiz).apply(W.zero)
    def monoid(implicit ev: Monoid[E]) = apply[E]
    def vector = apply[Vector[E]]
    def list = apply[List[E]]
    def set = apply[Set[E]]
  }
}


object DefaultAccumulatorHandler {
  def apply[E, W, Fx <: Accumulator[E]](fx: Fx)(implicit W: AccumZero[E, W]) = new fx.Unary[W, (W, ?)] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def purer[A](w: W, a: A): (W, A) = (w, a)

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
      def tell(e: E): P[Unit] =
        withLift { l => w0 =>
          pureInner((w0 |+ e, l.unitStash()))
        }

      def clear[A](scope: P[A]): P[A] =
        withLift { l => w0 =>
          l.run(scope)(W.zero).map { case (_, fa) => (w0, fa) }
        }
    }
  }.self
}

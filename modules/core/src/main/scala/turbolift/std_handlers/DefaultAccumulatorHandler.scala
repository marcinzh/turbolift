package turbolift.std_handlers
import cats.Monoid
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, AccumZero}
import turbolift.abstraction.implicits.{AccumSyntax, MonadParSyntax}
import turbolift.std_effects.{AccumulatorSig, Accumulator}


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

    def specialOps[M[_], U](context: ThisContext[M, U]) = new SpecialOps(context) with AccumulatorSig[U, E] {
      def tell(e: E): Unit !! U =
        withLift { l => w0 =>
          pureInner((w0 |+ e, l.unitStash()))
        }

      def clear[A](scope: A !! U): A !! U =
        withLift { l => w0 =>
          l.run(scope)(W.zero).map { case (_, fa) => (w0, fa) }
        }
    }
  }.self
}

package turbolift.std_handlers
import cats.Monoid
import cats.syntax.monoid._
import cats.syntax.functor._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, Accum}
import turbolift.abstraction.Implicits.MonadParSyntax
import turbolift.std_effects.{WriterSig, Writer}


object DefaultWriterHandler {
  def apply[W, Fx <: Writer[W]](fx: Fx)(implicit W: Monoid[W]) = new fx.Unary[W, (W, ?)] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def purer[A](w: W, a: A): (W, A) = (w, a)

      def flatMap[A, B](tma: W => M[(W, A)])(f: A => W => M[(W, B)]): W => M[(W, B)] =
        w0 => tma(w0).flatMap {
          case (w1, a) => f(a)(w1)
        }

      def zipPar[A, B](tma: W => M[(W, A)], tmb: W => M[(W, B)]): W => M[(W, (A, B))] =
        w0 => (tma(W.empty) *! tmb(W.empty)).map {
          case ((w1, a), (w2, b)) => ((w0 |+| w1) |+| w2, (a, b))
        }
    }

    def specialOps[M[_], U](context: ThisContext[M, U]) = new SpecialOps(context) with WriterSig[U, W] {
      def tell(w: W): Unit !! U =
        withLift { l => w0 =>
          pureInner((w0 |+| w, l.unitStash()))
        }

      def listen[A](scope: A !! U): (W, A) !! U =
        withLift { l => w0 =>
          l.run(scope)(W.empty).map { case (w, fa) => (w0 |+| w, fa.map((w, _))) }
        }

      def censor[A](scope: A !! U)(mod: W => W): A !! U =
        withLift { l => w0 =>
          l.run(scope)(W.empty).map { case (w, fa) => (w0 |+| mod(w), fa) }
        }

      def clear[A](scope: A !! U): A !! U =
        withLift { l => w0 =>
          l.run(scope)(W.empty).map { case (_, fa) => (w0, fa) }
        }
    }
  }.self
}

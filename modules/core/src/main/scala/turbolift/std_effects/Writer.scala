package turbolift.std_effects
import cats.Monoid
import cats.syntax.monoid._
import cats.syntax.functor._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.{MonadPar, Accum}
import turbolift.abstraction.implicits.MonadParSyntax


trait WriterSig[P[_], W] extends Signature[P] {
  def tell(w: W): P[Unit]
  def listen[A](scope: P[A]): P[(W, A)]
  def censor[A](scope: P[A])(mod: W => W): P[A]
  def clear[A](scope: P[A]): P[A]
}


trait Writer[W] extends Effect[WriterSig[?[_], W]] {
  def tell(w: W): Unit !! this.type = encodeFO(_.tell(w))
  def tell[X](x: X)(implicit ev: Accum[X, W]): Unit !! this.type = tell(ev.one(x))
  def listen[A, U](scope: A !! U): (W, A) !! U with this.type = encodeHO[U](run => _.listen(run(scope)))
  def censor[A, U](scope: A !! U)(f: W => W): A !! U with this.type = encodeHO[U](run => _.censor(run(scope))(f))
  def clear[A, U](scope: A !! U): A !! U with this.type = encodeHO[U](run => _.clear(run(scope)))

  def handler(implicit W: Monoid[W]) = DefaultWriterHandler[W, this.type](this).apply(W.empty)
}


object DefaultWriterHandler {
  def apply[W, Fx <: Writer[W]](fx: Fx)(implicit W: Monoid[W]) = new fx.Unary[W, (W, ?)] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      // def pure[A](a: A): W => M[(W, A)] = w => M.pure((w, a))

      def purer[A](w: W, a: A): (W, A) = (w, a)

      def lift[A](ma: M[A]): W => M[(W, A)] = w => ma.map((w, _))

      def flatMap[A, B](tma: W => M[(W, A)])(f: A => W => M[(W, B)]): W => M[(W, B)] =
        w0 => tma(w0).flatMap {
          case (w1, a) => f(a)(w1)
        }

      def zipPar[A, B](tma: W => M[(W, A)], tmb: W => M[(W, B)]): W => M[(W, (A, B))] =
        w0 => (tma(W.empty) *! tmb(W.empty)).map {
          case ((w1, a), (w2, b)) => ((w0 |+| w1) |+| w2, (a, b))
        }
    }

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with WriterSig[P, W] {
      def tell(w: W): P[Unit] = liftOuter(w0 => pureInner((w0 |+| w, ())))

      def listen[A](scope: P[A]): P[(W, A)] =
        withUnlift { run => w0 =>
          run(scope)(W.empty).map { case (w, fa) => (w0 |+| w, fa.map((w, _))) }
        }

      def censor[A](scope: P[A])(mod: W => W): P[A] =
        withUnlift { run => w0 =>
          run(scope)(W.empty).map { case (w, fa) => (w0 |+| mod(w), fa) }
        }

      def clear[A](scope: P[A]): P[A] =
        withUnlift { run => w0 =>
          run(scope)(W.empty).map { case (_, fa) => (w0, fa) }
        }
    }
  }.self
}

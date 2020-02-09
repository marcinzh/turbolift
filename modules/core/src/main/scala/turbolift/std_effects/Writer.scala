package turbolift.std_effects
import cats.Monoid
import cats.syntax.monoid._
import cats.syntax.functor._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.{MonadPar, Accum}
import turbolift.abstraction.implicits.MonadParSyntax


trait WriterSig[U, W] extends Signature[U] {
  def tell(w: W): Unit !! U
  def listen[A](scope: A !! U): (W, A) !! U
  def censor[A](scope: A !! U)(mod: W => W): A !! U
  def clear[A](scope: A !! U): A !! U
}


trait Writer[W] extends Effect[WriterSig[?, W]] {
  def tell(w: W): Unit !! this.type = encodeFO(_.tell(w))
  def tell[X](x: X)(implicit ev: Accum[X, W]): Unit !! this.type = tell(ev.one(x))
  def listen[A, U](scope: A !! U): (W, A) !! U with this.type = encodeHO[U](_.listen(scope))
  def censor[A, U](scope: A !! U)(f: W => W): A !! U with this.type = encodeHO[U](_.censor(scope)(f))
  def clear[A, U](scope: A !! U): A !! U with this.type = encodeHO[U](_.clear(scope))

  def handler(implicit W: Monoid[W]) = DefaultWriterHandler[W, this.type](this).apply(W.empty)
}


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

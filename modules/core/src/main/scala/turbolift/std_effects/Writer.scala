package turbolift.std_effects
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect._


trait WriterSig[W] extends Signature {
  def tell(w: W): Op[Unit]
}


trait Writer[W] extends Effect[WriterSig[W]] with WriterSig[W] {
  def tell(w: W): Unit !! this.type = encode(_.tell(w))
  def tell[X](x: X)(implicit ev: NonEmpty[X, W]): Unit !! this.type = tell(ev.nonEmpty(x))
  def listen[A, U](scope: A !! U)(implicit W: Monoid[W]) = handler.handle[U](scope).flatMap { case w_a @ (w, _) => tell(w) *>! pure(w_a) }
  def censor[A, U](scope: A !! U)(f: W => W)(implicit W: Monoid[W]) = handler.handle[U](scope).flatMap { case (w, a) => tell(f(w)) *>! pure(a) }

  def handler(implicit W: Monoid[W]) = WriterHandler[W, this.type](this).apply(W.empty)
}


object WriterHandler {
  def apply[W, Fx <: Writer[W]](effect: Fx)(implicit W: Monoid[W]) = new effect.Unary[W, (W, +?)] {
    def commonOps[M[+_] : MonadPar] = new CommonOps[M] {
      def lift[A](ma: M[A]): W => M[(W, A)] = w => ma.map((w, _))

      def flatMap[A, B](tma: W => M[(W, A)])(f: A => W => M[(W, B)]): W => M[(W, B)] =
        w0 => tma(w0).flatMap {
          case (w1, a) => f(a)(w1)
        }

      def zipPar[A, B](tma: W => M[(W, A)], tmb: W => M[(W, B)]): W => M[(W, (A, B))] =
        w0 => (tma(W.empty) *! tmb(W.empty)).map {
          case ((w1, a), (w2, b)) => ((w0 |@| w1) |@| w2, (a, b))
        }
    }

    def specialOps[M[+_] : MonadPar] = new SpecialOps[M] with WriterSig[W] {
      def tell(w: W) = w0 => Monad[M].pure((w0 |@| w, ()))
    }
  }.self
}

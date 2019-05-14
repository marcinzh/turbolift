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

  def handler(implicit W: Monoid[W]) = new DefaultHandler

  class DefaultHandler(implicit W: Monoid[W]) extends Nullary[(W, +?)] {
    def lift[M[_] : MonadPar, A](ma: M[A]): M[(W, A)] = ma.map((W.empty, _))

    def flatMap[M[_] : MonadPar, A, B](tma: M[(W, A)])(f: A => M[(W, B)]): M[(W, B)] =
      tma.flatMap { 
        case (w0, a) => f(a).map {
          case (w1, b) => (w0 |@| w1, b)
        }
      }

    def zipPar[M[_] : MonadPar, A, B](tma: M[(W, A)], tmb: M[(W, B)]): M[(W, (A, B))] =
      (tma *! tmb).map { case ((w0, a), (w1, b)) => ((w0 |@| w1), (a, b)) }

    def decode[M[+_] : MonadPar] = new Decode[M] with WriterSig[W] {
      def tell(w: W) = Monad[M].pure((w, ()))
    }
  }
}

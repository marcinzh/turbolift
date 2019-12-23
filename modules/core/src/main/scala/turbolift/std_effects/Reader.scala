package turbolift.std_effects
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect._


trait ReaderSig[R] extends Signature {
  def ask: Op[R]
}


trait Reader[R] extends Effect[ReaderSig[R]] with ReaderSig[R] {
  val ask = encode(_.ask)
  def asks[A](f: R => A) = ask.map(f)
  def local[A, U](mod: R => R)(scope: A !! U) = ask.flatMap(r => handler(mod(r)).handle[U](scope))

  val handler = ReaderHandler[R, this.type](this)
}


object ReaderHandler {
  def apply[R, Fx <: Reader[R]](effect: Fx) = new effect.Unary[R, Identity] {
    def commonOps[M[+_] : MonadPar] = new CommonOps[M] {
      def lift[A](ma: M[A]): R => M[A] = _ => ma

      def flatMap[A, B](tma: R => M[A])(f: A => R => M[B]): R => M[B] =
        r => tma(r).flatMap(a => f(a)(r))

      def zipPar[A, B](tma: R => M[A], tmb: R => M[B]): R => M[(A, B)] =
        r => tma(r) *! tmb(r)
    }

    def specialOps[M[+_] : MonadPar] = new SpecialOps[M] with ReaderSig[R] {
      val ask = r => MonadPar[M].pure(r)
    }
  }.self
}

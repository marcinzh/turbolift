package turbolift.abstraction.handlers
import mwords._
import turbolift.abstraction.effect.Signature


trait PrimitiveHandler[T[_[+_], +_]] { outer =>
  private[abstraction] def effectId: AnyRef

  def lift[M[+_] : MonadPar, A](ma: M[A]): T[M, A]
  def flatMap[M[+_] : MonadPar, A, B](tma: T[M, A])(f: A => T[M, B]): T[M, B]
  def zipPar[M[+_] : MonadPar, A, B](tma: T[M, A], tmb: T[M, B]): T[M, (A, B)]

  final def makeMonad[M[+_] : MonadPar] : MonadPar[T[M, +?]] = new MonadPar[T[M, +?]] {
    def pure[A](a: A): T[M, A] = outer.lift(MonadPar[M].pure(a))
    def flatMap[A, B](tma: T[M, A])(f: A => T[M, B]): T[M, B] = outer.flatMap(tma)(f)
    def zipPar[A, B](tma: T[M, A], tmb: T[M, B]): T[M, (A, B)] = outer.zipPar(tma, tmb)
  }

  trait Decode[M[+_]] extends Signature {
    type Op[+A] = T[M, A]
  }

  def decode[M[+_] : MonadPar] : Decode[M]

  final type Encoded[M[+_], A] = Decode[M] => Decode[M]#Op[A]

  val isFilterable: Boolean	
}

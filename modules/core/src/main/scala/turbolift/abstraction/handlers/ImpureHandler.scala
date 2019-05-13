package turbolift.abstraction.handlers
import mwords._
import turbolift.abstraction.effect.Signature


trait ImpureHandler[T[_[+_], +_]] {
  private[abstraction] def effectId: AnyRef

  def lift[M[+_] : MonadPar, A](ma: M[A]): T[M, A]
  def flatMap[M[+_] : MonadPar, A, B](tma: T[M, A])(f: A => T[M, B]): T[M, B]
  def zipPar[M[+_] : MonadPar, A, B](tma: T[M, A], tmb: T[M, B]): T[M, (A, B)]

  trait Decode[M[+_]] extends Signature {
    type Op[+A] = T[M, A]
  }

  def decode[M[+_] : MonadPar] : Decode[M]

  // def empty_?[M[+_] : MonadPar] : Option[T[M, Nothing]]
  val isFilterable: Boolean	
}

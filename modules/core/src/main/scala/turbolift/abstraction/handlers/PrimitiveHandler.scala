package turbolift.abstraction.handlers
import mwords._
import turbolift.abstraction.effect.Signature


trait PrimitiveHandler[T[_[+_], +_]] { outer =>
  private[abstraction] def effectId: AnyRef

  def commonOps[M[+_] : MonadPar] : CommonOps[M]

  abstract class CommonOps[M[+_] : MonadPar] extends MonadPar[T[M, +?]] {
    final type TM[+A] = T[M, A]
    override def pure[A](a: A): TM[A] = lift(MonadPar[M].pure(a))
    def lift[A](ma: M[A]): TM[A]
  }

  def specialOps[M[+_] : MonadPar] : SpecialOps[M]

  trait SpecialOps[M[+_]] extends Signature {
    type Op[+A] = T[M, A]
  }

  final type Encoded[M[+_], A] = SpecialOps[M] => SpecialOps[M]#Op[A]

  val isFilterable: Boolean	
}

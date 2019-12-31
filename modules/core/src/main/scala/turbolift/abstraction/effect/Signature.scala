package turbolift.abstraction.effect
import mwords._
import turbolift.abstraction.!!


trait Signature[P[_]] {
  implicit def theMonad: MonadPar[P]
}

object Signature {
  type Fun[P[_], A] = Signature[P] => P[A]
}

trait FailSig[P[_]] extends Signature[P] {
  def fail[A]: P[A]
}

private[abstraction] object FailSig {
  // def encodeFail[P[_]] = (sig: FailSig[P]) => sig.fail
  def encodeFail[P[_], A] = encodeFail_.asInstanceOf[FailSig[P] => P[A]]
  private val encodeFail_ = (sig: FailSig[AnyEffect#Phantom]) => sig.fail
}

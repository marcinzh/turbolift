package turbolift.abstraction.effect
import mwords.MonadPar


trait Signature[P[_]] {
  implicit def theMonad: MonadPar[P]
}

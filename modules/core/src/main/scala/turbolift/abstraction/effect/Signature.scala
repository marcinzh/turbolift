package turbolift.abstraction.effect
import turbolift.abstraction.typeclass.MonadPar


trait Signature[P[_]] {
  implicit def theMonad: MonadPar[P]
}

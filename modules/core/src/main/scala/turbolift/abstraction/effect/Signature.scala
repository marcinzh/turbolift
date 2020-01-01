package turbolift.abstraction.effect
import mwords.MonadPar
import turbolift.abstraction.!!


trait Signature[P[_]] {
  implicit def theMonad: MonadPar[P]
}

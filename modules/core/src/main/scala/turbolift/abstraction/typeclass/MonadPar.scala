package turbolift.abstraction.typeclass
// import simulacrum.{typeclass, op}
import cats.{Monad, Defer}



trait MonadPar[F[_]] extends Monad[F] with ZipPar[F] with Defer[F] {
}

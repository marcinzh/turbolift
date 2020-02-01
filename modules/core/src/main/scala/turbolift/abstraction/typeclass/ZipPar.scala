package turbolift.abstraction.typeclass
// import simulacrum.typeclass
// import simulacrum.{typeclass, op}
// import cats.Functor


trait ZipPar[F[_]] {
  def zipPar[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}
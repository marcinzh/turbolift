package turbolift.abstraction.internals.interpreter
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar


trait Control[T[_[_], _], M[_], F[_], U] {
  val mainMonad: MonadPar[T[M, *]]
  val innerMonad: MonadPar[M]
  val lifting: Lifting[* !! U, T[M, *], F]

  final type ThisLiftOps = LiftOps[* !! U, T[M, *], F]
  final def withLift[A](ff: ThisLiftOps => T[M, F[A]]): A !! U = lifting.withLift(ff)
  final def pureInner[A](a: A): M[A] = innerMonad.pure(a)
}

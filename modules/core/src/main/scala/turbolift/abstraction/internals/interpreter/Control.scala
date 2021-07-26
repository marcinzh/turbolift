package turbolift.abstraction.internals.interpreter
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar


trait Control[T[_[_], _], M[_], F[_], U]:
  val mainMonad: MonadPar[T[M, _]]
  val innerMonad: MonadPar[M]
  val lifting: Lifting[!![_, U], T[M, _], F]

  final type ThisLiftOps = LiftOps[!![_, U], T[M, _], F]
  final def withLift[A](ff: ThisLiftOps => T[M, F[A]]): A !! U = lifting.withLift(ff)
  final def pureInner[A](a: A): M[A] = innerMonad.pure(a)

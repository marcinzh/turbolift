package turbolift.abstraction.internals.interpreter
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar


trait Context[U] {
  type Main[A]
  type Inner[A]
  type Stash[A]

  val mainMonad: MonadPar[Main]
  val innerMonad: MonadPar[Inner]
  val lifting: Lifting[? !! U, Main, Stash]
}

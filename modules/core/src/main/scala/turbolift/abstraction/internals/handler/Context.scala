package turbolift.abstraction.internals.handler
import turbolift.abstraction.typeclass.MonadPar


trait Context {
  type Main[A]
  type Inner[A]
  type Outer[A]
  type Stash[A]

  val mainMonad: MonadPar[Main]
  val innerMonad: MonadPar[Inner]
  val outerMonad: MonadPar[Outer]
  val lifting: Lifting2[Outer, Main, Stash]
}

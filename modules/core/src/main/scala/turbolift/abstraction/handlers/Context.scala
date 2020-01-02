package turbolift.abstraction.handlers
import mwords.{MonadPar, ~>}
import turbolift.abstraction.!!


trait Context {
  type Main[A]
  type Inner[A]
  type Outer[A]
  type Stash[A]

  val mainMonad: MonadPar[Main]
  val innerMonad: MonadPar[Inner]
  val outerMonad: MonadPar[Outer]
  val lifting: Lifting[Outer, Main, Stash]
}

package turbolift.abstraction.internals.interpreter
import cats.{Functor, Id}


trait Lifting[Outer[_], Inner[_], Stash[_]] {
  type ThisLiftOps = LiftOps[Outer, Inner, Stash]
  val stashFunctor: Functor[Stash]
  def withLift[A](ff: ThisLiftOps => Inner[Stash[A]]): Outer[A]
}

object Lifting {
  def identity[F[_]]: Lifting[F, F, Id] =
    new Lifting[F, F, Id] {
      override val stashFunctor: Functor[Id] = Functor[Id]
      override def withLift[A](ff: ThisLiftOps => F[A]): F[A] = ff(LiftOps.identity[F])
    }

  def compose[Outer[_], Middle[_], Inner[_], StashO[_], StashI[_]](
    outer: Lifting[Outer, Middle, StashO],
    inner: Lifting[Middle, Inner, StashI],
  ): Lifting[Outer, Inner, Lambda[X => StashI[StashO[X]]]] = {
    type Stash[A] = StashI[StashO[A]]
    new Lifting[Outer, Inner, Stash] {
      override val stashFunctor: Functor[Stash] = inner.stashFunctor.compose(outer.stashFunctor)
      override def withLift[A](ff: ThisLiftOps => Inner[Stash[A]]): Outer[A] =
        outer.withLift { outerOps =>
          inner.withLift { innerOps =>
            ff(LiftOps.compose(outerOps, innerOps))
          }
        }
    }
  }
}

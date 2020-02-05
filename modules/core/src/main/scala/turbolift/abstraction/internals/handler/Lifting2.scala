package turbolift.abstraction.internals.handler
import cats.{Functor, Id, ~>}


trait LiftOps[Outer[_], Inner[_], Stash[_]] {
  def run[A](oa: Outer[A]): Inner[Stash[A]]
  def pure[A](a: A): Stash[A]
}

trait Lifting2[Outer[_], Inner[_], Stash[_]] {
  type ThisLiftOps = LiftOps[Outer, Inner, Stash]
  def stashFunctor: Functor[Stash]
  def withLift[A](ff: ThisLiftOps => Inner[Stash[A]]): Outer[A]
}

object Lifting2 {
  def compose[Outer[_], Middle[_], Inner[_], StashO[_], StashI[_]](outer: Lifting2[Outer, Middle, StashO], inner: Lifting2[Middle, Inner, StashI]): Lifting2[Outer, Inner, Lambda[X => StashI[StashO[X]]]] = {
    type Stash[A] = StashI[StashO[A]]

    new Lifting2[Outer, Inner, Stash] {
      def stashFunctor: Functor[Stash] = inner.stashFunctor.compose(outer.stashFunctor)
  
      def withLift[A](ff: ThisLiftOps => Inner[Stash[A]]): Outer[A] =
        outer.withLift { ffOuter =>
          inner.withLift { ffInner =>
            ff(new ThisLiftOps {
              def run[A](oa: Outer[A]): Inner[Stash[A]] = ffInner.run(ffOuter.run(oa))
              def pure[A](a: A): Stash[A] = ffInner.pure(ffOuter.pure(a))
            })
          }
        }
    }
  }


  def identity[M[_]]: Lifting2[M, M, Id] =
    new Lifting2[M, M, Id] {
      def stashFunctor: Functor[Id] = Functor[Id]

      def withLift[A](ff: ThisLiftOps => M[A]): M[A] = ff(liftOpsVal)

      private val liftOpsVal: ThisLiftOps = new ThisLiftOps {
        def run[A](ma: M[A]): M[A] = ma
        def pure[A](a: A): A = a
      }
    }
}

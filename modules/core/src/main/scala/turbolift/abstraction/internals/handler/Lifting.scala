package turbolift.abstraction.internals.handler
import cats.{Functor, Id, ~>}


trait LiftOps[Outer[_], Inner[_], Stash[_]] {
  def run[A](oa: Outer[A]): Inner[Stash[A]]
  def pureStash[A](a: A): Stash[A]
  def unitStash(): Stash[Unit]
}

trait Lifting[Outer[_], Inner[_], Stash[_]] {
  type ThisLiftOps = LiftOps[Outer, Inner, Stash]
  def stashFunctor: Functor[Stash]
  def withLift[A](ff: ThisLiftOps => Inner[Stash[A]]): Outer[A]
}

object Lifting {
  def compose[Outer[_], Middle[_], Inner[_], StashO[_], StashI[_]](outer: Lifting[Outer, Middle, StashO], inner: Lifting[Middle, Inner, StashI]): Lifting[Outer, Inner, Lambda[X => StashI[StashO[X]]]] = {
    type Stash[A] = StashI[StashO[A]]

    new Lifting[Outer, Inner, Stash] {
      def stashFunctor: Functor[Stash] = inner.stashFunctor.compose(outer.stashFunctor)
  
      def withLift[A](ff: ThisLiftOps => Inner[Stash[A]]): Outer[A] =
        outer.withLift { ffOuter =>
          inner.withLift { ffInner =>
            ff(new ThisLiftOps {
              def run[A](oa: Outer[A]): Inner[Stash[A]] = ffInner.run(ffOuter.run(oa))
              def pureStash[A](a: A): Stash[A] = ffInner.pureStash(ffOuter.pureStash(a))
              def unitStash(): Stash[Unit] = ffInner.pureStash(ffOuter.unitStash())
            })
          }
        }
    }
  }


  def identity[M[_]]: Lifting[M, M, Id] =
    new Lifting[M, M, Id] {
      def stashFunctor: Functor[Id] = Functor[Id]

      def withLift[A](ff: ThisLiftOps => M[A]): M[A] = ff(liftOpsVal)

      private val liftOpsVal: ThisLiftOps = new ThisLiftOps {
        def run[A](ma: M[A]): M[A] = ma
        def pureStash[A](a: A): A = a
        def unitStash(): Unit = ()
      }
    }
}

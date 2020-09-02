package turbolift.abstraction.internals.interpreter
import cats.Id


trait LiftOps[Outer[_], Inner[_], Stash[_]] {
  def run[A](oa: Outer[A]): Inner[Stash[A]]
  def pureStash[A](a: A): Stash[A]
  def unitStash(): Stash[Unit]
}

object LiftOps {
  def identity[F[_]] : LiftOps[F, F, Id] = identityVal.asInstanceOf[LiftOps[F, F, Id]]

  private val identityVal: LiftOps[Id, Id, Id] =
    new LiftOps[Id, Id, Id] {
      def run[A](a: A): A = a
      def pureStash[A](a: A): A = a
      def unitStash(): Unit = ()
    }

  def compose[Outer[_], Middle[_], Inner[_], StashO[_], StashI[_]](
    outer: LiftOps[Outer, Middle, StashO],
    inner: LiftOps[Middle, Inner, StashI],
  ): LiftOps[Outer, Inner, Lambda[X => StashI[StashO[X]]]] = {
    type Stash[A] = StashI[StashO[A]]
    new LiftOps[Outer, Inner, Stash] {
      def run[A](oa: Outer[A]): Inner[Stash[A]] = inner.run(outer.run(oa))
      def pureStash[A](a: A): Stash[A] = inner.pureStash(outer.pureStash(a))
      def unitStash(): Stash[Unit] = inner.pureStash(outer.unitStash())
    }
  }
}

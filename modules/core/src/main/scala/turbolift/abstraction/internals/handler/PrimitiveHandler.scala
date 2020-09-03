package turbolift.abstraction.internals.handler
import cats.{Functor, ~>}
import turbolift.abstraction.!!
import turbolift.abstraction.internals.effect.HasEffectId
import turbolift.abstraction.typeclass.MonadPar

import PrimitiveHandler_toplevel._
object PrimitiveHandler_toplevel {
  val ~> = cats.arrow.FunctionK
}


trait PrimitiveHandlerStub extends HasEffectId.Delegate {
  type ThisSignature[U] <: AnyRef
}


sealed trait PrimitiveHandler[T[_[_], _], O[_]] extends PrimitiveHandlerStub {
  def theFunctor: Functor[O]

  def commonOps[M[_]: MonadPar] : SuperCommonOps[M]

  abstract class SuperCommonOps[M[_]: MonadPar] extends MonadPar[T[M, ?]] with Lifting[T[M, ?], M, O] {
    final def mainMonad: MonadPar[T[M, ?]] = this
    final def innerMonad: MonadPar[M] = MonadPar[M]
    final val stashFunctor: Functor[O] = theFunctor
  }

  final type ThisContext[M[_], U] = Context[U] {
    type Main[A] = T[M, A]
    type Inner[A] = M[A]
  }

  def specialOps[M[_], U](context: ThisContext[M, U]): SuperSpecialOps[M, U]

  abstract class SuperSpecialOps[M[_], U](val context: ThisContext[M, U]) { this: ThisSignature[U] =>
    final type Stash[A] = context.Stash[A]

    final implicit def innerMonad: MonadPar[M] = context.innerMonad
    final /******/ def mainMonad: MonadPar[T[M, ?]] = context.mainMonad
    final implicit def stashFunctor: Functor[Stash] = context.lifting.stashFunctor

    final def toSignature: Signature[U] = this

    final def pureInner[A](a: A): M[A] = context.innerMonad.pure(a)

    type ThisLiftOps = LiftOps[? !! U, T[M, ?], Stash] //// same as: context.lifting.ThisLiftOps
    def withLift[A](ff: ThisLiftOps => T[M, Stash[A]]): A !! U = context.lifting.withLift(ff)
  }
}


object PrimitiveHandler {
  trait Nullary[O[_]] extends PrimitiveHandler[Lambda[(`M[_]`, A) => M[O[A]]], O] {
    abstract class CommonOps[M[_]](implicit M: MonadPar[M]) extends SuperCommonOps[M] {
      def purer[A](a: A): O[A]
      final override def pure[A](a: A): M[O[A]] = M.pure(purer(a))
      final override def defer[A](tma: => M[O[A]]): M[O[A]] = M.defer(tma)
      final override def withLift[A](ff: ThisLiftOps => M[O[A]]): M[O[A]] = ff(liftOpsVal)

      private val unitStashVal = purer(())
      private val liftOpsVal = new ThisLiftOps {
        def run[A](tma: M[O[A]]): M[O[A]] = tma
        def pureStash[A](a: A): O[A] = purer(a)
        def unitStash(): O[Unit] = unitStashVal
      }
    }

    abstract class SpecialOps[M[_], U](ctx: ThisContext[M, U]) extends SuperSpecialOps(ctx) { this: ThisSignature[U] =>
      final override type ThisLiftOps = LiftOps[? !! U, Lambda[X => M[O[X]]], Stash]
      final override def withLift[A](ff: ThisLiftOps => M[O[Stash[A]]]): A !! U = context.lifting.withLift(ff)
    }
  }

  trait Unary[S, O[_]] extends PrimitiveHandler[Lambda[(`M[_]`, A) => S => M[O[A]]], O] {
    abstract class CommonOps[M[_]](implicit M: MonadPar[M]) extends SuperCommonOps[M] {
      def purer[A](s: S, a: A): O[A]
      final override def pure[A](a: A): S => M[O[A]] = s => M.pure(purer(s, a))
      final override def defer[A](tma: => S => M[O[A]]): S => M[O[A]] = s => M.defer(tma(s))
      final override def withLift[A](ff: ThisLiftOps => M[O[A]]): S => M[O[A]] =
        s => ff(new ThisLiftOps {
          def run[A](tma: S => M[O[A]]): M[O[A]] = tma(s)
          def pureStash[A](a: A): O[A] = purer(s, a)
          def unitStash(): O[Unit] = purer(s, ())
        })
    }

    abstract class SpecialOps[M[_], U](ctx: ThisContext[M, U]) extends SuperSpecialOps(ctx) { this: ThisSignature[U] =>
      final override type ThisLiftOps = LiftOps[? !! U, Lambda[X => S => M[O[X]]], Stash]
      final override def withLift[A](ff: ThisLiftOps => S => M[O[Stash[A]]]): A !! U = context.lifting.withLift(ff)
    }
  }
}

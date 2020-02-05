package turbolift.abstraction.internals.handler
import cats.{Functor, ~>}
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Signature, HasEffectId}
import turbolift.abstraction.typeclass.MonadPar

import PrimitiveHandler_toplevel._
object PrimitiveHandler_toplevel {
  val ~> = cats.arrow.FunctionK
}

trait PrimitiveHandlerStub extends HasEffectId.Delegate {
  val isFilterable: Boolean

  type ThisSignature[P[_]] <: Signature[P]
}


sealed trait PrimitiveHandler[T[_[_], _], O[_]] extends PrimitiveHandlerStub {
  def theFunctor: Functor[O]

  def commonOps[M[_]: MonadPar] : CommonOps[M]

  abstract class CommonOps[M[_]: MonadPar] extends MonadPar[T[M, ?]] with Lifting[T[M, ?], M, O] {
    final def mainMonad: MonadPar[T[M, ?]] = this
    final def innerMonad: MonadPar[M] = MonadPar[M]
    final def stashFunctor: Functor[O] = theFunctor
  }

  final type ThisContext[M[_], P[_]] = Context {
    type Main[A] = T[M, A]
    type Inner[A] = M[A]
    type Outer[A] = P[A]
  }

  def specialOps[M[_], P[_]](context: ThisContext[M, P]): SpecialOps[M, P]

  abstract class SpecialOps[M[_], P[_]](val context: ThisContext[M, P]) extends Signature[P] { this: ThisSignature[P] =>
    final type Stash[A] = context.Stash[A]

    final override def theMonad = outerMonad //// Signature's monad
    
    final implicit def innerMonad: MonadPar[M] = context.innerMonad
    final implicit def outerMonad: MonadPar[P] = context.outerMonad
    final /******/ def mainMonad: MonadPar[T[M, ?]] = context.mainMonad
    final implicit def stashFunctor: Functor[Stash] = context.lifting.stashFunctor

    final def pureInner[A](a: A): M[A] = context.innerMonad.pure(a)
    final def pureOuter[A](a: A): P[A] = context.outerMonad.pure(a)

    type ThisLiftOps = LiftOps[P, T[M, ?], Stash] //// same as: context.lifting.ThisLiftOps
    final def withLift[A](ff: ThisLiftOps => T[M, Stash[A]]): P[A] = context.lifting.withLift(ff)
  }
}


object PrimitiveHandler {
  trait Nullary[O[_]] extends PrimitiveHandler[Lambda[(`M[_]`, A) => M[O[A]]], O] {
    abstract class CommonOps[M[_]](implicit M: MonadPar[M]) extends super.CommonOps[M] {
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
  }

  trait Unary[S, O[_]] extends PrimitiveHandler[Lambda[(`M[_]`, A) => S => M[O[A]]], O] {
    abstract class CommonOps[M[_]](implicit M: MonadPar[M]) extends super.CommonOps[M] {
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
  }
}

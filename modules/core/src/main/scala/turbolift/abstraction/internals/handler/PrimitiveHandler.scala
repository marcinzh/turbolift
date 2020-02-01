package turbolift.abstraction.internals.handler
import mwords.MonadPar
import cats.{Functor, ~>}
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Signature, HasEffectId}

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

    final def defaultPure[A](a: A): T[M, A] = lift(MonadPar[M].pure(a))
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
    final def liftOuter[A](tma: T[M, A]): P[A] = context.lifting.lift(tma)

    //// Overriden in direct subclasses of SpecialOps, but with exactly the same values as here, except 
    //// for having type T inlined. This inlining appearently prevents compiler from getting confused
    //// about finding Monad[M] instance, in effect definitions.
    type Unlift = context.lifting.Unlift
    def withUnlift[A](ff: Unlift => T[M, Stash[A]]): P[A] = context.lifting.withUnlift(ff)
  }
}


object PrimitiveHandler {
  trait Nullary[O[_]] extends PrimitiveHandler[Lambda[(`M[_]`, A) => M[O[A]]], O] {
    abstract class CommonOps[M[_]: MonadPar] extends super.CommonOps[M] {
      final override def defer[A](tma: => M[O[A]]): M[O[A]] = MonadPar[M].defer(tma)
      final override def withUnlift[A](ff: Unlift => M[O[A]]): M[O[A]] =
        ff(~>.id[Lambda[X => M[O[X]]]])
    }

    abstract class SpecialOps[M[_], P[_]](ctx: ThisContext[M, P]) extends super.SpecialOps(ctx) { this: ThisSignature[P] =>
      final override type Unlift = P ~> Lambda[X => M[O[Stash[X]]]]
      final override def withUnlift[A](ff: Unlift => M[O[Stash[A]]]): P[A] = context.lifting.withUnlift(ff)
    }
  }

  trait Unary[S, O[_]] extends PrimitiveHandler[Lambda[(`M[_]`, A) => S => M[O[A]]], O] {
    abstract class CommonOps[M[_]: MonadPar] extends super.CommonOps[M] {
      final override def defer[A](tma: => S => M[O[A]]): S => M[O[A]] = s => MonadPar[M].defer(tma(s))
      final override def withUnlift[A](ff: Unlift => M[O[A]]): S => M[O[A]] =
        s => ff(new Unlift {
          def apply[A](tma: S => M[O[A]]): M[O[A]] = tma(s)
        })
    }

    abstract class SpecialOps[M[_], P[_]](ctx: ThisContext[M, P]) extends super.SpecialOps(ctx) { this: ThisSignature[P] =>
      final override type Unlift = P ~> Lambda[X => S => M[O[Stash[X]]]]
      final override def withUnlift[A](ff: Unlift => S => M[O[Stash[A]]]): P[A] = context.lifting.withUnlift(ff)
    }
  }
}

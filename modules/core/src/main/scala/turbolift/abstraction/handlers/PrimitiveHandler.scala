package turbolift.abstraction.handlers
import mwords._
import turbolift.abstraction.effect.Signature
import turbolift.abstraction.!!


trait PrimitiveHandlerBase {
  private[abstraction] val effectId: AnyRef
  val isFilterable: Boolean

  type ThisSignature[P[_]] <: Signature[P]
}


trait PrimitiveHandler[T[_[_], _], O[_]] extends PrimitiveHandlerBase {
  def theFunctor: Functor[O]

  def commonOps[M[_] : MonadPar] : CommonOps[M]

  abstract class CommonOps[M[_] : MonadPar] extends MonadPar[T[M, ?]] with Lifting[T[M, ?], M, O] {
    final def mainMonad: MonadPar[T[M, ?]] = this
    final def innerMonad: MonadPar[M] = MonadPar[M]
    final def stashFunctor: Functor[O] = theFunctor

    override def pure[A](a: A): T[M, A] = lift(MonadPar[M].pure(a))
  }

  final type ThisContext[M[_], P[_]] = Context {
    type Main[A] = T[M, A]
    type Inner[A] = M[A]
    type Outer[A] = P[A]
  }

  def specialOps[M[_], P[_]](context: ThisContext[M, P]): SpecialOps[M, P]

  abstract class SpecialOps[M[_], P[_]](val context: ThisContext[M, P]) extends Signature[P] { this: ThisSignature[P] =>
    final type Stash[A] = context.Stash[A]
    
    final implicit def innerMonad: MonadPar[M] = context.innerMonad
    final implicit def outerMonad: MonadPar[P] = context.outerMonad
    final /*implicit*/ def mainMonad: MonadPar[T[M, ?]] = context.mainMonad
    final implicit def stashFunctor: Functor[Stash] = context.lifting.stashFunctor

    final def liftOuter[A](tma: T[M, A]): P[A] = context.lifting.lift(tma)
    final def pureInner[A](a: A): M[A] = context.innerMonad.pure(a)

    final override def theMonad = outerMonad

    // moved to subclasses to aid compiler find innerMonad instance:
    // final type Unlift = context.Unlift
    // def withUnlift[A](ff: Unlift => TM[Stash[A]]): A !! U = context.withUnlift(ff)
  }
}


object PrimitiveHandler {
  trait Nullary[O[_]] extends PrimitiveHandler[Lambda[(`M[_]`, A) => M[O[A]]], O] {
    abstract class CommonOps[M[_] : MonadPar] extends super.CommonOps[M] {
      final override def withUnlift[A](ff: Unlift => M[O[A]]): M[O[A]] =
        ff(~>.identity[Lambda[X => M[O[X]]]])
    }

    abstract class SpecialOps[M[_], P[_]](ctx: ThisContext[M, P]) extends super.SpecialOps(ctx) { this: ThisSignature[P] =>
      final type Unlift = P ~> Lambda[X => M[O[Stash[X]]]]
      final def withUnlift[A](ff: Unlift => M[O[Stash[A]]]): P[A] = context.lifting.withUnlift(ff)
    }
  }

  trait Unary[S, O[_]] extends PrimitiveHandler[Lambda[(`M[_]`, A) => S => M[O[A]]], O] {
    abstract class CommonOps[M[_] : MonadPar] extends super.CommonOps[M] {
      final override def withUnlift[A](ff: Unlift => M[O[A]]): S => M[O[A]] =
        s => ff(new Unlift {
          def apply[A](tma: S => M[O[A]]): M[O[A]] = tma(s)
        })
    }

    abstract class SpecialOps[M[_], P[_]](ctx: ThisContext[M, P]) extends super.SpecialOps(ctx) { this: ThisSignature[P] =>
      final type Unlift = P ~> Lambda[X => S => M[O[Stash[X]]]]
      final def withUnlift[A](ff: Unlift => S => M[O[Stash[A]]]): P[A] = context.lifting.withUnlift(ff)
    }
  }
}

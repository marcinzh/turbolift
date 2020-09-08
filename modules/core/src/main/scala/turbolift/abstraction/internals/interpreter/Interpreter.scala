package turbolift.abstraction.internals.interpreter
import cats.Id
import turbolift.abstraction.internals.effect.HasEffectId
import turbolift.abstraction.{Handler, HandlerCases}


trait HasSignature extends HasEffectId.Delegate {
  type Signature[U] <: AnyRef
}

// trait HasResult {
//   type Result[A]
// }

// trait HasTrans extends HasResult {
//   type Trans[M[_], A]
//   private[abstraction] def transformer: MonadTransformer[Trans, Result]
// }

sealed trait Interpreter extends HasSignature {
  type Result[A]
  type ElimEffect
  type IntroEffect

  final type ThisHandler = Handler[Result, ElimEffect, IntroEffect]
}


object Interpreter {
  type Saturated[O[_], L, N] = InterpreterCases.Saturated {
    type Result[A] = O[A]
    type ElimEffect = L
    type IntroEffect = N
  }
}


object InterpreterCases {
  trait Unsealed extends Interpreter

  sealed trait Saturated extends Interpreter {
    final def toHandler: ThisHandler = HandlerCases.Primitive[Result, ElimEffect, IntroEffect](this)
  }

  trait Dependent[TargetEffect] extends Saturated {
    final override type Result[A] = A
    final override type IntroEffect = TargetEffect
    
    def interpret[U <: TargetEffect]: Signature[U]
  }

  trait Independent extends Interpreter {
    final override type IntroEffect = Any
  }

  trait HasTrans extends Independent {
    type Trans[M[_], A]
    private[abstraction] def transformer: MonadTransformer[Trans, Result]
  }

  sealed trait SaturatedTrans extends HasTrans with Saturated {
    private[abstraction] def prime[M[_], A](tma: Trans[M, A]): M[Result[A]]
  }

  trait SaturatedNullary[O[_]] extends SaturatedTrans {
    final override type Result[A] = O[A]
    final override type Trans[M[_], A] = M[O[A]]
    private[abstraction] final override def prime[M[_], A](tma: M[O[A]]): M[O[A]] = tma
  }

  final class SaturatedUnary[S, O[_], L](initial: S, unsaturated: UnsaturatedUnary[S, O]) extends SaturatedTrans {
    override def effectIdDelegate = unsaturated
    override type Result[A] = O[A]
    override type Trans[M[_], A] = S => M[O[A]]
    override type ElimEffect = L
    private[abstraction] final override def prime[M[_], A](tma: S => M[O[A]]): M[O[A]] = tma(initial)
    private[abstraction] final override def transformer: MonadTransformer[Trans, Result] = unsaturated.transformer
  }

  trait UnsaturatedUnary[S, O[_]] extends HasTrans {
    final override type Result[A] = O[A]
    final override type Trans[M[_], A] = S => M[O[A]]
    final def toHandler(s: S): ThisHandler = saturate(s).toHandler
    final def saturate(s: S) = new SaturatedUnary[S, O, ElimEffect](s, this)
  }
}

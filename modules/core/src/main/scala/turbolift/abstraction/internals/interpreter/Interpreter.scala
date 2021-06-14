package turbolift.abstraction.internals.interpreter
import turbolift.abstraction.{Handler, HandlerCases}


sealed trait Interpreter extends HasSignature:
  type Result[A]
  type ElimEffect
  type IntroEffect

  final type ThisHandler = Handler[Result, ElimEffect, IntroEffect]


object Interpreter:
  type Saturated[O[_], L, N] = InterpreterCases.Saturated {
    type Result[A] = O[A]
    type ElimEffect = L
    type IntroEffect = N
  }


object InterpreterCases:
  trait Unsealed extends Interpreter

  sealed trait Saturated extends Interpreter:
    final def toHandler: ThisHandler = HandlerCases.Primitive[Result, ElimEffect, IntroEffect](this)


  trait Proxy[TargetEffect] extends Saturated:
    final override type Result[A] = A
    final override type IntroEffect = TargetEffect

    def onOperation[U <: TargetEffect]: Signature[U]


  trait HasTrans extends Interpreter:
    final override type IntroEffect = Any
    type Trans[M[_], A]
    private[abstraction] def transformer: MonadTransformer[Trans, Result]


  sealed trait SaturatedTrans extends HasTrans with Saturated:
    private[abstraction] def prime[M[_], A](tma: Trans[M, A]): M[Result[A]]


  trait SaturatedStateless[O[_]] extends SaturatedTrans:
    final override type Result[A] = O[A]
    final override type Trans[M[_], A] = M[O[A]]
    private[abstraction] final override def prime[M[_], A](tma: M[O[A]]): M[O[A]] = tma


  final class SaturatedStateful[S, O[_], L](initial: S, unsaturated: UnsaturatedStateful[S, O]) extends SaturatedTrans:
    override def effectIdDelegate = unsaturated
    override type Result[A] = O[A]
    override type Trans[M[_], A] = S => M[O[A]]
    override type ElimEffect = L
    private[abstraction] final override def prime[M[_], A](tma: S => M[O[A]]): M[O[A]] = tma(initial)
    private[abstraction] final override def transformer: MonadTransformer[Trans, Result] = unsaturated.transformer


  trait UnsaturatedStateful[S, O[_]] extends HasTrans:
    final override type Result[A] = O[A]
    final override type Trans[M[_], A] = S => M[O[A]]
    final def toHandler(s: S): ThisHandler = new SaturatedStateful[S, O, ElimEffect](s, this).toHandler

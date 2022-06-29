package turbolift.internals.interpreter
import cats.Functor
import turbolift.{!!, Signature}
import turbolift.{Handler, HandlerCases}
import turbolift.typeclass.MonadZip


sealed trait Interpreter extends Signature:
  type Result[+A]
  type IntroEffect

  final type ThisHandler = Handler[Result, ThisEffect, IntroEffect]

  private[turbolift] val signatures: Array[Signature]


object Interpreter:
  type Apply[F[+_], L, N] = Interpreter {
    type Result[+A] = F[A]
    type ThisEffect = L
    type IntroEffect = N
  }


object InterpreterCases:
  private[turbolift] trait Unsealed extends Interpreter //// subclassed by Effect

  sealed trait Proxy extends Interpreter:
    final override type Result[A] = A
    final override type !@![+A, U] = A !! (U & IntroEffect)
    final def toHandler: ThisHandler = HandlerCases.Primitive[Result, ThisEffect, IntroEffect](this, ())


  private[turbolift] trait ProxyWithParam[Fx] extends Proxy: //// subclassed by Effect
    final override type IntroEffect = Fx


  sealed trait Flow extends Interpreter:
    final override type IntroEffect = Any
    type Initial
    type Trans[_[_], _]

    final type ThisControlBound[U] = Control_!![Trans, U]
    type ThisControl[U] >: ThisControlBound[U] <: ThisControlBound[U]
    final override type !@![A, U] = (kk: ThisControl[U]) ?=> Trans[kk.LowerMonad, kk.UpperFunctor[A]]

    def onPure[A](a: A): Trans[[X] =>> X, A]
    def onFlatMap[A, B, M[_]: MonadZip](tma: Trans[M, A])(f: A => Trans[M, B]): Trans[M, B]
    def onZip[A, B, M[_]: MonadZip](tma: Trans[M, A], tmb: Trans[M, B]): Trans[M, (A, B)]

    private[turbolift] final def transform[M[_]](M: MonadZip[M]): MonadZip[Trans[M, _]] = new:
      override def pure[A](a: A): Trans[M, A] = liftish(onPure(a))(M)
      override def flatMap[A, B](tma: Trans[M, A])(f: A => Trans[M, B]): Trans[M, B] = onFlatMap(tma)(f)(M)
      override def zip[A, B](tma: Trans[M, A], tmb: Trans[M, B]): Trans[M, (A, B)] = onZip(tma, tmb)(M)

    private[turbolift] val resultFunctor: Functor[Result]
    private[turbolift] def prime[M[_], A](initial: Initial, tma: Trans[M, A]): M[Result[A]]
    private[turbolift] def liftish[M[_], A](ta: Trans[[X] =>> X, A])(M: MonadZip[M]): Trans[M, A]
    private[turbolift] def layer[I <: InverseControl](that: I): that.Layer[Trans, Result]
    private[turbolift] final def focus[M[_]](M: MonadZip[M]): InverseControl.Focus[Trans, M] = InverseControl.focus(M)



  abstract class Stateless[F[+_]: Functor] extends Flow: //// subclassed by Effect
    final override type Initial = Unit
    final override type Result[+A] = F[A]
    final override type Trans[M[_], X] = M[F[X]]
    final override type ThisControl[U] = Control_!![[M[_], X] =>> M[F[X]], U]

    final def toHandler: ThisHandler = HandlerCases.Primitive[Result, ThisEffect, IntroEffect](this, ())

    private[turbolift] final override val resultFunctor: Functor[F] = summon[Functor[F]]
    private[turbolift] final override def prime[M[_], A](initial: Unit, tma: Trans[M, A]): M[F[A]] = tma
    private[turbolift] final override def liftish[M[_], A](fa: F[A])(M: MonadZip[M]): Trans[M, A] = M.pure(fa)

    private[turbolift] final override def layer[I <: InverseControl](that: I): that.Layer[Trans, Result] =
      new that.LayerImpl[Trans, Result]:
        override def upperFunctor: Functor[UpperFunctor] = that.upperFunctor compose resultFunctor
        override def withControl[A](ff: this.ThisControl => FocusMonad[UpperFunctor[A]]): UpperMonad[A] =
          that.withControl { kk =>
            ff(new ThisControlImpl:
              override def inner[A](a: A): UpperFunctor[A] = kk.inner(onPure(a))
              override def locally[A](body: UpperMonad[A]): FocusMonad[UpperFunctor[A]] = kk.locally(body)
            )
          }


  abstract class Stateful[S, F[+_]: Functor] extends Flow: //// subclassed by Effect
    final override type Initial = S
    final override type Result[+A] = F[A]
    final override type Trans[M[_], X] = S => M[F[X]]
    final override type ThisControl[U] = Control_!![[M[_], X] =>> S => M[F[X]], U]

    final def toHandler(initial: S): ThisHandler = HandlerCases.Primitive[Result, ThisEffect, IntroEffect](this, initial)

    private[turbolift] final override val resultFunctor: Functor[F] = summon[Functor[F]]
    private[turbolift] final override def prime[M[_], A](s: S, tma: Trans[M, A]): M[Result[A]] = tma(s)
    private[turbolift] final override def liftish[M[_], A](ffa: S => F[A])(M: MonadZip[M]): Trans[M, A] = s => M.pure(ffa(s))

    private[turbolift] final override def layer[I <: InverseControl](that: I): that.Layer[Trans, Result] =
      new that.LayerImpl[Trans, Result]:
        override def upperFunctor: Functor[UpperFunctor] = that.upperFunctor compose resultFunctor
        override def withControl[A](ff: this.ThisControl => FocusMonad[UpperFunctor[A]]): UpperMonad[A] =
          s => that.withControl { kk =>
            ff(new ThisControlImpl:
              override def inner[A](a: A): UpperFunctor[A] = kk.inner(onPure(a)(s))
              override def locally[A](body: UpperMonad[A]): FocusMonad[UpperFunctor[A]] = kk.locally(body(s))
            )
          }

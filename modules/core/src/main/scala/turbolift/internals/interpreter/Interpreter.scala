package turbolift.internals.interpreter
import cats.Functor
import turbolift.{!!, Signature}
import turbolift.{Handler, HandlerCases}
import turbolift.typeclass.MonadZip


/** Super trait for any user-defined [[Interpreter Interpreter]].
  *
  * There are 2 kinds of interpreters:
  * - [[Interpreter.Proxy Proxy]] interpreters - The easy way: handle this effect in terms of other, preexisting effects (a.k.a. reinterpretation).
  * - [[Interpreter.Flow Flow]] interpreters - The hard way: implement a monad transformer for this effect. Take full [[Control]].
  *
  * Interpreters aren't directly usable. A [[turbolift.Handler Handler]] must be created from the interpreter, by calling `toHandler` method.
  */
sealed trait Interpreter extends Signature:
  type Result[+A]
  type IntroEffect

  /** Alias for [[Handler]], specialized for this interperter. */
  final type ThisHandler = Handler[Result, ThisEffect, IntroEffect]

  private[turbolift] val signatures: Array[Signature]


object Interpreter:
  private[turbolift] type Apply[F[+_], L, N] = Interpreter {
    type Result[+A] = F[A]
    type ThisEffect = L
    type IntroEffect = N
  }

  private[turbolift] trait Unsealed extends Interpreter //// subclassed by Effect

  /** Super trait for any user-defined proxy [[Interpreter]].
    *
    * [[Proxy]] translates operations of this effect, into operations of some other effects (dependencies). This is also known as "reinterpretation" in some effect systems.
    * 
    * Unlike [[Flow]] interpreters, [[Proxy]] interpreters have their [[Result]] type fixed to identity.
    * 
    * @tparam Fx Type-level set of effects, expressed as an intersection type, specifying dependencies of this proxy interpreter.
    */
  trait Proxy[Fx] extends Interpreter:
    final override type Result[A] = A
    final override type IntroEffect = Fx
    final override type !@![+A, U] = A !! (U & IntroEffect)

    /** Creates a [[turbolift.Handler Handler]] from this interpreter. */
    final def toHandler: ThisHandler = HandlerCases.Primitive[Result, ThisEffect, IntroEffect](this, ())


  /** Super trait for [[Stateless]] and [[Stateful]] interpreters. */
  sealed trait Flow extends Interpreter:
    final override type IntroEffect = Any
    type Initial
    type Trans[_[_], _]

    private final type ThisControlBound[U] = Control.Apply[Trans, U]
    type ThisControl[U] >: ThisControlBound[U] <: ThisControlBound[U]
    final override type !@![A, U] = (kk: ThisControl[U]) ?=> Trans[kk.LowerMonad, kk.UpperFunctor[A]]

    def onPure[A](a: A): Trans[[X] =>> X, A]
    def onFlatMap[A, B, M[_]: MonadZip](tma: Trans[M, A])(f: A => Trans[M, B]): Trans[M, B]
    def onZip[A, B, M[_]: MonadZip](tma: Trans[M, A], tmb: Trans[M, B]): Trans[M, (A, B)]

    private[internals] final def transform[M[_]](M: MonadZip[M]): MonadZip[Trans[M, _]] = new:
      override def pure[A](a: A): Trans[M, A] = liftish(onPure(a))(M)
      override def flatMap[A, B](tma: Trans[M, A])(f: A => Trans[M, B]): Trans[M, B] = onFlatMap(tma)(f)(M)
      override def zip[A, B](tma: Trans[M, A], tmb: Trans[M, B]): Trans[M, (A, B)] = onZip(tma, tmb)(M)

    private[internals] val resultFunctor: Functor[Result]
    private[internals] def prime[M[_], A](initial: Initial, tma: Trans[M, A]): M[Result[A]]
    private[internals] def liftish[M[_], A](ta: Trans[[X] =>> X, A])(M: MonadZip[M]): Trans[M, A]
    private[internals] def layer[I <: InverseControl](that: I): that.Layer[Trans, Result]
    private[internals] final def focus[M[_]](M: MonadZip[M]): InverseControl.Focus[Trans, M] = InverseControl.focus(M)


  /** Super class for any user-defined stateless [[Interpreter Interpreter]].
    * 
    * Works by translating operations of this effect, into a monad, defined by transformer: `T[M, A] = M[F[A]]`.
    * 
    * Implementation must provide: 
    * - Definitions for `pure`, `flatMap` and `zip` for the transformed monad, given `MonadZip` instance for the inner monad.
    * - `Functor` instance for `F[_]`
    * 
    * @tparam F Part of this interpreter's monad transformer.
    */

  abstract class Stateless[F[+_]: Functor] extends Flow: //// subclassed by Effect
    final override type Initial = Unit
    final override type Result[+A] = F[A]
    final override type Trans[M[_], X] = M[F[X]]
    final override type ThisControl[U] = Control.Apply[[M[_], X] =>> M[F[X]], U]

    /** Creates a [[turbolift.Handler Handler]] from this interpreter. */
    final def toHandler: ThisHandler = HandlerCases.Primitive[Result, ThisEffect, IntroEffect](this, ())

    private[internals] final override val resultFunctor: Functor[F] = summon[Functor[F]]
    private[internals] final override def prime[M[_], A](initial: Unit, tma: Trans[M, A]): M[F[A]] = tma
    private[internals] final override def liftish[M[_], A](fa: F[A])(M: MonadZip[M]): Trans[M, A] = M.pure(fa)

    private[internals] final override def layer[I <: InverseControl](that: I): that.Layer[Trans, Result] =
      new that.LayerImpl[Trans, Result]:
        override def upperFunctor: Functor[UpperFunctor] = that.upperFunctor compose resultFunctor
        override def withControl[A](ff: this.ThisControl => FocusMonad[UpperFunctor[A]]): UpperMonad[A] =
          that.withControl { kk =>
            ff(new ThisControlImpl:
              override def inner[A](a: A): UpperFunctor[A] = kk.inner(onPure(a))
              override def locally[A](body: UpperMonad[A]): FocusMonad[UpperFunctor[A]] = kk.locally(body)
            )
          }

  /** Super class for any user-defined stateful [[Interpreter Interpreter]].
    * 
    * Works by translating operations of this effect, into a monad, defined by transformer: `T[M, A] = S => M[F[A]]`.
    * 
    * Implementation must provide: 
    * - Definitions for `pure`, `flatMap` and `zip` for the transformed monad, given [[turbolift.typeclass.MonadZip MonadZip]] instance for the inner monad.
    * - `Functor` instance for `F[_]`
    * 
    * @tparam S Part of this interpreter's monad transformer.
    * @tparam F Part of this interpreter's monad transformer.
    */
  abstract class Stateful[S, F[+_]: Functor] extends Flow: //// subclassed by Effect
    final override type Initial = S
    final override type Result[+A] = F[A]
    final override type Trans[M[_], X] = S => M[F[X]]
    final override type ThisControl[U] = Control.Apply[[M[_], X] =>> S => M[F[X]], U]

    /** Creates a [[turbolift.Handler Handler]] from this interpreter. */
    final def toHandler(initial: S): ThisHandler = HandlerCases.Primitive[Result, ThisEffect, IntroEffect](this, initial)

    private[internals] final override val resultFunctor: Functor[F] = summon[Functor[F]]
    private[internals] final override def prime[M[_], A](s: S, tma: Trans[M, A]): M[Result[A]] = tma(s)
    private[internals] final override def liftish[M[_], A](ffa: S => F[A])(M: MonadZip[M]): Trans[M, A] = s => M.pure(ffa(s))

    private[internals] final override def layer[I <: InverseControl](that: I): that.Layer[Trans, Result] =
      new that.LayerImpl[Trans, Result]:
        override def upperFunctor: Functor[UpperFunctor] = that.upperFunctor compose resultFunctor
        override def withControl[A](ff: this.ThisControl => FocusMonad[UpperFunctor[A]]): UpperMonad[A] =
          s => that.withControl { kk =>
            ff(new ThisControlImpl:
              override def inner[A](a: A): UpperFunctor[A] = kk.inner(onPure(a)(s))
              override def locally[A](body: UpperMonad[A]): FocusMonad[UpperFunctor[A]] = kk.locally(body(s))
            )
          }

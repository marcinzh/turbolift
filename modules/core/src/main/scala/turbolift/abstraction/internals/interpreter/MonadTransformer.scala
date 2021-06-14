package turbolift.abstraction.internals.interpreter
import cats.{Functor, ~>}
import turbolift.abstraction.!!
import turbolift.abstraction.{IHandler, HandlerCases}
import turbolift.abstraction.internals.effect.HasEffectId
import turbolift.abstraction.typeclass.MonadPar


sealed trait MonadTransformer[T[_[_], _], O[_]] extends HasSignature:
  private[abstraction] val theFunctor: Functor[O]
  private[abstraction] def lifting[M[_]]: Lifting[T[M, _], M, O]

  def onTransform[M[_]: MonadPar]: MonadPar[T[M, _]]
  def onOperation[M[_], F[_], U](implicit ctrl: ThisControl[M, F, U]): Signature[U]

  type ThisControl[M[_], F[_], U] = Control[T, M, F, U]

  @inline final implicit def innerMonad[M[_], F[_], U](implicit ctrl: ThisControl[M, F, U]): MonadPar[M] = ctrl.innerMonad
  @inline final implicit def stashFunctor[M[_], F[_], U](implicit ctrl: ThisControl[M, F, U]): Functor[F] = ctrl.lifting.stashFunctor


object MonadTransformerCases:
  abstract class Stateless[O[_]: Functor] extends MonadTransformer[[M[_], A] =>> M[O[A]], O] with InterpreterCases.SaturatedStateless[O]:
    def onReturn[A](a: A): O[A]

    override def onTransform[M[_]: MonadPar]: Transformed[M]

    abstract class Transformed[M[_]](implicit M: MonadPar[M]) extends MonadPar[[X] =>> M[O[X]]]:
      final override def pure[A](a: A): M[O[A]] = M.pure(onReturn(a))
      final override def defer[A](tma: => M[O[A]]): M[O[A]] = M.defer(tma)

    private[abstraction] final override val theFunctor = Functor[O]
    private[abstraction] final override def transformer: MonadTransformer[Trans, Result] = this

    private[abstraction] final override def lifting[M[_]] = new Lifting[[X] =>> M[O[X]], M, O]:
      override val stashFunctor: Functor[O] = theFunctor
      override def withLift[A](ff: ThisLiftOps => M[O[A]]): M[O[A]] = ff(liftOpsVal)
      private val unitStashVal = onReturn(())
      private val liftOpsVal = new ThisLiftOps:
        override def run[A](tma: M[O[A]]): M[O[A]] = tma
        override def pureStash[A](a: A): O[A] = onReturn(a)
        override def unitStash(): O[Unit] = unitStashVal


  abstract class Stateful[S, O[_]: Functor] extends MonadTransformer[[M[_], A] =>> S => M[O[A]], O] with InterpreterCases.UnsaturatedStateful[S, O]:
    def onReturn[A](s: S, a: A): O[A]

    override def onTransform[M[_]: MonadPar]: Transformed[M]

    abstract class Transformed[M[_]](implicit M: MonadPar[M]) extends MonadPar[[X] =>> S => M[O[X]]]:
      final override def pure[A](a: A): S => M[O[A]] = s => M.pure(onReturn(s, a))
      final override def defer[A](tma: => S => M[O[A]]): S => M[O[A]] = s => M.defer(tma(s))

    private[abstraction] final override val theFunctor = Functor[O]
    private[abstraction] final override def transformer: MonadTransformer[Trans, Result] = this

    private[abstraction] final override def lifting[M[_]] = new Lifting[[X] =>> S => M[O[X]], M, O]:
      override val stashFunctor: Functor[O] = theFunctor
      override def withLift[A](ff: ThisLiftOps => M[O[A]]): S => M[O[A]] =
        s => ff(new ThisLiftOps:
          override def run[A](tma: S => M[O[A]]): M[O[A]] = tma(s)
          override def pureStash[A](a: A): O[A] = onReturn(s, a)
          override def unitStash(): O[Unit] = onReturn(s, ())
        )

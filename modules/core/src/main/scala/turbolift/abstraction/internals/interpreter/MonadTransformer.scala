package turbolift.abstraction.internals.interpreter
import cats.{Functor, ~>}
import turbolift.abstraction.!!
import turbolift.abstraction.{Handler, HandlerCases}
import turbolift.abstraction.internals.effect.HasEffectId
import turbolift.abstraction.typeclass.MonadPar


sealed trait MonadTransformer[T[_[_], _], O[_]] extends InterpreterCases.Unsealed {
  final override type Result[A] = O[A]

  private[abstraction] def theFunctor: Functor[O]

  private[abstraction] def lifting[M[_]]: Lifting[T[M, ?], M, O]

  def transform[M[_]: MonadPar]: MonadPar[T[M, ?]]

  def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]): Signature[U]

  type ThisContext[M[_], F[_], U] = Context[T, M, F, U]

  @inline final implicit def innerMonad[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]): MonadPar[M] = ctx.innerMonad
  @inline final implicit def stashFunctor[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]): Functor[F] = ctx.lifting.stashFunctor
}


object MonadTransformerCases {
  trait Nullary[O[_]] extends MonadTransformer[Lambda[(`M[_]`, A) => M[O[A]]], O] {
    def purer[A](a: A): O[A]

    override def transform[M[_]: MonadPar]: Transformed[M]

    abstract class Transformed[M[_]](implicit M: MonadPar[M]) extends MonadPar[Lambda[X => M[O[X]]]] {
      final override def pure[A](a: A): M[O[A]] = M.pure(purer(a))
      final override def defer[A](tma: => M[O[A]]): M[O[A]] = M.defer(tma)
    }

    private[abstraction] final override def lifting[M[_]] = new Lifting[Lambda[X => M[O[X]]], M, O] {
      override val stashFunctor: Functor[O] = theFunctor
      override def withLift[A](ff: ThisLiftOps => M[O[A]]): M[O[A]] = ff(liftOpsVal)
      private val unitStashVal = purer(())
      private val liftOpsVal = new ThisLiftOps {
        override def run[A](tma: M[O[A]]): M[O[A]] = tma
        override def pureStash[A](a: A): O[A] = purer(a)
        override def unitStash(): O[Unit] = unitStashVal
      }
    }

    final def toHandler: Handler[O, ElimEffect] = HandlerCases.Nullary[O, ElimEffect](this)
  }


  trait Unary[S, O[_]] extends MonadTransformer[Lambda[(`M[_]`, A) => S => M[O[A]]], O] {
    def purer[A](s: S, a: A): O[A]

    override def transform[M[_]: MonadPar]: Transformed[M]

    abstract class Transformed[M[_]](implicit M: MonadPar[M]) extends MonadPar[Lambda[X => S => M[O[X]]]] {
      final override def pure[A](a: A): S => M[O[A]] = s => M.pure(purer(s, a))
      final override def defer[A](tma: => S => M[O[A]]): S => M[O[A]] = s => M.defer(tma(s))
    }

    private[abstraction] final override def lifting[M[_]] = new Lifting[Lambda[X => S => M[O[X]]], M, O] {
      override val stashFunctor: Functor[O] = theFunctor
      override def withLift[A](ff: ThisLiftOps => M[O[A]]): S => M[O[A]] =
        s => ff(new ThisLiftOps {
          override def run[A](tma: S => M[O[A]]): M[O[A]] = tma(s)
          override def pureStash[A](a: A): O[A] = purer(s, a)
          override def unitStash(): O[Unit] = purer(s, ())
        })
    }

    final def toHandler(s: S): Handler[O, ElimEffect] = HandlerCases.Unary[S, O, ElimEffect](this, s)
  }
}

package turbolift.abstraction.internals.engine
import turbolift.abstraction.!!
import turbolift.abstraction.internals.effect.HasEffectId
import turbolift.abstraction.internals.interpreter.{MonadTransformer, Lifting, AnySignature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.ComputationCases.Done

/*
P[_] - entire monad stack
Q[_] - some inner monad in the stack, selected as lifting target
T[_[_], _] - transformer being used to grow the stack
M[_] - monad to which T is being applied, which is either a former P, or a foreign base monad (e.g. IO)
O[_] - covariant part of T
F[_] - composed covariant parts of all T's **above** the monad selected as lifting target
*/

sealed trait TransformerStack[P[_]] extends HasEffectId.Delegate:
  def outerMonad: MonadPar[P]
  def decoder[U](recur: [X] => X !! U => P[X]): AnySignature[U]
  def pushNext[T[_[_], _], O[_]](transformer: MonadTransformer[T, O]): TransformerStack[T[P, _]]


object TransformerStack:
  def pushFirst[T[_[_], _], O[_], M[_]: MonadPar](transformer: MonadTransformer[T, O]): TransformerStack[T[M, _]] =
    TransformerStackCases.PushFirst(transformer)


private object TransformerStackCases:
  sealed trait CanLift[P[_], Q[_], F[_]] extends TransformerStack[P]:
    def lifting: Lifting[P, Q, F]
    def canDecode: CanDecode[Q]

    final override def decoder[U](recur: [X] => X !! U => P[X]): AnySignature[U] =
      canDecode.makeDecoder[P, F, U](recur, lifting)(outerMonad)

    final override def pushNext[T[_[_], _], O[_]](transformer: MonadTransformer[T, O]): TransformerStack[T[P, _]] =
      PushNext(this, transformer, canDecode)


  sealed trait CanDecode[Q[_]] extends CanLift[Q, Q, [X] =>> X]:
    final override def lifting = Lifting.identity[Q]
    final override def canDecode: CanDecode[Q] = this

    def makeDecoder[P[_]: MonadPar, F[_], U](recur: [X] => X !! U => P[X], lifting: Lifting[P, Q, F]): AnySignature[U]


  final case class PushNext[T[_[_], _], O[_], P[_], Q[_], F[_]](
    that: CanLift[P, Q, F],
    transformer: MonadTransformer[T, O],
    override val canDecode: CanDecode[Q],
  ) extends CanLift[T[P, _], Q, [X] =>> F[O[X]]]:
    override def outerMonad: MonadPar[T[P, _]] = transformer.onTransform[P](that.outerMonad)
    override def effectIdDelegate: HasEffectId = that
    override val lifting = Lifting.compose(transformer.lifting, that.lifting)


  final case class PushFirst[T[_[_], _], O[_], M[_]: MonadPar](transformer: MonadTransformer[T, O]) extends CanDecode[T[M, _]]:
    enclosing =>
    override def effectIdDelegate: HasEffectId = transformer
    override def outerMonad: MonadPar[T[M, _]] = transformer.onTransform[M]

    override def makeDecoder[P[_]: MonadPar, F[_], U](recur: [X] => X !! U => P[X], lifting: Lifting[P, T[M, _], F]): AnySignature[U] =
      val lifting2 = new Lifting[!![_, U], T[M, _], F]:
        override val stashFunctor = lifting.stashFunctor
        override def withLift[A](ff: ThisLiftOps => T[M, F[A]]): A !! U =
          Done(lifting.withLift { l =>
            ff(new ThisLiftOps:
              override def run[A](ua: A !! U): T[M, F[A]] = l.run(recur(ua))
              override def pureStash[A](a: A): F[A] = l.pureStash(a)
              override def unitStash(): F[Unit] = l.unitStash()
            )
          })

      val control = new transformer.ThisControl[M, F, U]:
        override val mainMonad: MonadPar[T[M, _]] = enclosing.outerMonad
        override val innerMonad: MonadPar[M] = MonadPar[M]
        override val lifting: Lifting[!![_, U], T[M, _], F] = lifting2

      transformer.onOperation(control)

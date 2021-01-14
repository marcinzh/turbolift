package turbolift.abstraction.internals.engine
import cats.{Id, ~>}
import turbolift.abstraction.!!
import turbolift.abstraction.internals.effect.HasEffectId
import turbolift.abstraction.internals.interpreter.{MonadTransformer, Lifting, AnySignature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.ComputationCases.Done


sealed trait TransformerStack[P[_]] extends HasEffectId.Delegate {
  def outerMonad: MonadPar[P]
  def decoder[U](recur: (? !! U) ~> P): AnySignature[U]
  def pushNext[T[_[_], _], O[_]](transformer: MonadTransformer[T, O]): TransformerStack[T[P, *]]
}


object TransformerStack {
  def pushFirst[T[_[_], _], O[_], M[_]: MonadPar](transformer: MonadTransformer[T, O]): TransformerStack[T[M, *]] =
    TransformerStackCases.PushFirst(transformer)
}


private object TransformerStackCases {
  sealed trait CanLift[P[_], Q[_], F[_]] extends TransformerStack[P] {
    def lifting: Lifting[P, Q, F]
    def canDecode: CanDecode[Q]

    final override def decoder[U](recur: (? !! U) ~> P): AnySignature[U] =
      canDecode.makeDecoder(recur, lifting)(outerMonad)

    final override def pushNext[T[_[_], _], O[_]](transformer: MonadTransformer[T, O]): TransformerStack[T[P, *]] =
      PushNext(this, transformer, canDecode)
  }


  sealed trait CanDecode[Q[_]] extends CanLift[Q, Q, Id] {
    final override def lifting = Lifting.identity[Q]
    final override def canDecode: CanDecode[Q] = this

    def makeDecoder[P[_]: MonadPar, F[_], U](recur: (? !! U) ~> P, lifting: Lifting[P, Q, F]): AnySignature[U]
  }


  final case class PushNext[T[_[_], _], O[_], P[_], Q[_], F[_]](
    that: CanLift[P, Q, F],
    transformer: MonadTransformer[T, O],
    override val canDecode: CanDecode[Q],
  ) extends CanLift[T[P, *], Q, Lambda[X => F[O[X]]]] {
    override def outerMonad: MonadPar[T[P, *]] = transformer.transform[P](that.outerMonad)
    override def effectIdDelegate: HasEffectId = that
    override val lifting = Lifting.compose(transformer.lifting, that.lifting)
  }


  final case class PushFirst[T[_[_], _], O[_], M[_]: MonadPar](transformer: MonadTransformer[T, O]) extends CanDecode[T[M, *]] { outer =>
    override def effectIdDelegate: HasEffectId = transformer
    override def outerMonad: MonadPar[T[M, *]] = transformer.transform[M]

    override def makeDecoder[P[_]: MonadPar, F[_], U](recur: (? !! U) ~> P, lifting: Lifting[P, T[M, *], F]): AnySignature[U] = {
      val lifting2 = new Lifting[? !! U, T[M, *], F] {
        override val stashFunctor = lifting.stashFunctor
        override def withLift[A](ff: ThisLiftOps => T[M, F[A]]): A !! U =
          Done(lifting.withLift { l =>
            ff(new ThisLiftOps {
              override def run[A](ua: A !! U): T[M, F[A]] = l.run(recur(ua))
              override def pureStash[A](a: A): F[A] = l.pureStash(a)
              override def unitStash(): F[Unit] = l.unitStash()
            })
          })
      }

      val context = new transformer.ThisContext[M, F, U] {
        override val mainMonad: MonadPar[T[M, *]] = outer.outerMonad
        override val innerMonad: MonadPar[M] = MonadPar[M]
        override val lifting: Lifting[? !! U, T[M, *], F] = lifting2
      }

      transformer.interpret(context)
    }
  }
}

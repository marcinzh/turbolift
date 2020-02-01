package turbolift.abstraction.internals.interpreter
import mwords.{MonadPar}
import cats.Id
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{HasEffectId, Signature}
import turbolift.abstraction.internals.handler.{PrimitiveHandler, Lifting, Context}


sealed trait HandlerStack[P[_]] extends HasEffectId.Delegate {
  def outerMonad: MonadPar[P]
  def decoder: Signature[P]
  def pushNext[T[_[_], _], O[_]](primitive: PrimitiveHandler[T, O]): HandlerStack[T[P, ?]]
}


object HandlerStack {
  def pushFirst[T[_[_], _], O[_], M[_]: MonadPar](primitive: PrimitiveHandler[T, O]): HandlerStack[T[M, ?]] =
    HandlerStackCases.PushFirst(primitive)    
}


private object HandlerStackCases {
  sealed trait CanLift[P[_], Q[_], F[_]] extends HandlerStack[P] {
    def lifting: Lifting[P, Q, F]
    def canDecode: CanDecode[Q]

    final override def decoder: Signature[P] = canDecode.makeDecoder(lifting)(outerMonad)

    final override def pushNext[T[_[_], _], O[_]](primitive: PrimitiveHandler[T, O]): HandlerStack[T[P, ?]] =
      PushNext(this, primitive, canDecode)
  }


  sealed trait CanDecode[Q[_]] extends CanLift[Q, Q, Id] {
    final override def lifting = Lifting.identity[Q]
    final override def canDecode: CanDecode[Q] = this

    def makeDecoder[P[_]: MonadPar, F[_]](penthouse: Lifting[P, Q, F]): Signature[P]
  }


  final case class PushNext[T[_[_], _], O[_], P[_], Q[_], F[_]](
    that: CanLift[P, Q, F],
    primitive: PrimitiveHandler[T, O],
    override val canDecode: CanDecode[Q],
  ) extends CanLift[T[P, ?], Q, Lambda[X => F[O[X]]]] {
    private val commonOps = primitive.commonOps[P](that.outerMonad)
    override def outerMonad: MonadPar[T[P, ?]] = commonOps
    override def effectIdDelegate: HasEffectId = that
    override val lifting = Lifting.compose(commonOps, that.lifting)
  }


  final case class PushFirst[T[_[_], _], O[_], M[_]: MonadPar](primitive: PrimitiveHandler[T, O]) extends CanDecode[T[M, ?]] { outer =>
    override def effectIdDelegate: HasEffectId = primitive
    override def outerMonad: MonadPar[T[M, ?]] = primitive.commonOps[M]

    override def makeDecoder[P[_]: MonadPar, F[_]](penthouse: Lifting[P, T[M, ?], F]): Signature[P] = {
      val context: primitive.ThisContext[M, P] =
        new Context {
          override type Main[A] = T[M, A]
          override type Inner[A] = M[A]
          override type Outer[A] = P[A]
          override type Stash[A] = F[A]
          override val mainMonad: MonadPar[Main] = outer.outerMonad
          override val innerMonad: MonadPar[Inner] = MonadPar[M]
          override val outerMonad: MonadPar[Outer] = MonadPar[P]
          override val lifting: Lifting[Outer, Main, Stash] = penthouse
        }

      primitive.specialOps(context)
    }
  }
}

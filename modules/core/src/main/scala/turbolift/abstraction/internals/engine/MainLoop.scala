package turbolift.abstraction.internals.engine
import cats.{Id, ~>}
import turbolift.abstraction.{!!, ComputationCases, HandlerCases}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.internals.effect.EffectId
import turbolift.abstraction.internals.interpreter.{AnySignature, MonadTransformer}
import turbolift.std_effects.ChoiceSig


final class MainLoop[M[_], U](
  val theMonad: MonadPar[M],
  val handlerStacks: List[HandlerStack[M]],
  val vmt: Array[AnyRef],
) extends ((? !! U) ~> M) {
  override def apply[A](ua: A !! U): M[A] = loop(ua, Que.empty)

  private def loop[A](ua: A !! U, que: Que[M, U]): M[A] = {
    import ComputationCases._
    import QueCases._
    def castS[Z[U1] <: AnySignature[U1]](f: Z[U] => A !! U) = f.asInstanceOf[AnySignature[U] => A !! U]
    def castU[A1](ua1: A1 !! U) = ua1.asInstanceOf[A !! U]
    def castM[A1](ma1: M[A1]) = ma1.asInstanceOf[M[A]]

    ua match {
      case Pure(a) => loop(Done(theMonad.pure(a)), que)
      case ua: Done[tA, tM, U] =>
        val ma: M[A] = ua.value.asInstanceOf[M[A]]
        que match {
          case Empty() => ma
          case SeqStep(f, next) => theMonad.flatMap(ma)(a => loop(castU(f(a)), next))
          case ParStepLeft(ux, next) => loop(castU(ux), ParStepRight(ma, next))
          case ParStepRight(mx, next) => loop(Done(castM(theMonad.zipPar(mx, ma))), next)
        }
      case Defer(th) => theMonad.defer(loop(th(), que))
      case FlatMap(ux, k) => loop(castU(ux), SeqStep(k, que))
      case ZipPar(ux, uy) => loop(castU(ux), ParStepLeft(castU(uy), que))
      case Dispatch(id, op) => loop(castS(op)(lookup(id)), que)
      case Scope(ux, h) => loop(Done(h.prime(push(h.transformer).apply(ux))), que)
    }
  }

  private def lookup(id: EffectId): AnySignature[U] = Vmt.lookup(vmt, id).asInstanceOf[AnySignature[U]]

  def push[T[_[_], _], O[_], V](transformer: MonadTransformer[T, O]): MainLoop[T[M, ?], U with V] = {
    val newHead: HandlerStack[T[M, ?]] = HandlerStack.pushFirst(transformer)(this.theMonad)
    val newTail: List[HandlerStack[T[M, ?]]] = this.handlerStacks.map(_.pushNext(transformer))
    val newStack: List[HandlerStack[T[M, ?]]] = newHead :: newTail
    val newVmt = Vmt.prealloc(newStack.size)
    val newLoop = new MainLoop[T[M, ?], U with V](newHead.outerMonad, newStack, newVmt)
    Vmt.fill[EffectId, AnySignature[U with V], HandlerStack[T[M, ?]]](
      newVmt,
      newStack,
      _.effectId,
      _.decoder(newLoop),
      _.isInstanceOf[ChoiceSig[_]] //@#@TODO call it only on the new trans
    )
    newLoop
  }
}


object MainLoop {
  val pure: MainLoop[Trampoline, Any] = apply(TrampolineInstances.monad)
  val pureStackUnsafe: MainLoop[Id, Any] = apply(MonadPar.identity)
  def apply[M[_]: MonadPar]: MainLoop[M, Any] = new MainLoop[M, Any](MonadPar[M], Nil, Vmt.empty)
}

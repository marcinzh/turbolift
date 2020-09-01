package turbolift.abstraction.internals.engine
import cats.{Id, ~>}
import turbolift.abstraction.{!!, ComputationCases}
import turbolift.abstraction.internals.effect.{EffectId, Signature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.internals.handler.PrimitiveHandler
import turbolift.std_effects.ChoiceSig


final class MainLoop[M[_], U](
  val theMonad: MonadPar[M],
  val handlerStacks: List[HandlerStack[M]],
  val vmt: Array[AnyRef],
) extends ((? !! U) ~> M) {
  override def apply[A](ua: A !! U): M[A] = run(ua, Que.empty)

  private def run[A](ua: A !! U, que: Que[M, U]): M[A] = {
    import ComputationCases._
    import QueCases._
    def castS[Z[U1] <: Signature[U1]](f: Z[U] => A !! U) = f.asInstanceOf[Signature[U] => A !! U]
    def castU[A1](ua1: A1 !! U) = ua1.asInstanceOf[A !! U]
    def castM[A1](ma1: M[A1]) = ma1.asInstanceOf[M[A]]

    ua match {
      case Pure(a) => run(Done(theMonad.pure(a)), que)
      case ua: Done[tA, tM, U] =>
        val ma: M[A] = ua.value.asInstanceOf[M[A]]
        que match {
          case Empty() => ma
          case SeqStep(f, next) => theMonad.flatMap(ma)(a => run(castU(f(a)), next))
          case ParStepLeft(ux, next) => run(castU(ux), ParStepRight(ma, next))
          case ParStepRight(mx, next) => run(Done(castM(theMonad.zipPar(mx, ma))), next)
        }
      case Defer(th) => theMonad.defer(run(th(), que))
      case FlatMap(ux, k) => run(castU(ux), SeqStep(k, que))
      case ZipPar(ux, uy) => run(castU(ux), ParStepLeft(castU(uy), que))
      case Dispatch(id, op) => run(castS(op)(lookup(id)), que)
      case Scope(ux, h) => run(Done(castM(h.prime(push(h.primitive).apply(ux)))), que)
    }
  }

  private def lookup(id: EffectId): Signature[U] = Vmt.lookup(vmt, id).asInstanceOf[Signature[U]]

  def push[T[_[_], _], O[_], V](primitive: PrimitiveHandler[T, O]): MainLoop[T[M, ?], U with V] = {
    val newHead: HandlerStack[T[M, ?]] = HandlerStack.pushFirst(primitive)(this.theMonad)
    val newTail: List[HandlerStack[T[M, ?]]] = this.handlerStacks.map(_.pushNext(primitive))
    val newStack: List[HandlerStack[T[M, ?]]] = newHead :: newTail
    val newVmt = Vmt.prealloc(newStack.size)
    val newLoop = new MainLoop[T[M, ?], U with V](newHead.outerMonad, newStack, newVmt)
    Vmt.fill[EffectId, Signature[U with V], HandlerStack[T[M, ?]]](
      newVmt,
      newStack,
      _.effectId,
      _.decoder(newLoop),
      _.isInstanceOf[ChoiceSig[_]]
    )
    newLoop
  }
}


object MainLoop {
  val pure: MainLoop[Trampoline, Any] = apply(TrampolineInstances.monad)
  val pureStackUnsafe: MainLoop[Id, Any] = apply(MonadPar.identity)
  def apply[M[_]: MonadPar]: MainLoop[M, Any] = new MainLoop[M, Any](MonadPar[M], Nil, Vmt.empty)
}

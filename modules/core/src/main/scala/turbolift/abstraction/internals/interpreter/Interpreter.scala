package turbolift.abstraction.internals.interpreter
import cats.{Id, ~>}
import turbolift.abstraction.{!!, ComputationCases}
import turbolift.abstraction.effect.{EffectId, Signature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.internals.handler.PrimitiveHandler


final class Interpreter[M[_], U](
  val theMonad: MonadPar[M],
  val handlerStacks: List[HandlerStack[M]],
  val failEffectId: EffectId,
) extends ((? !! U) ~> M) {
  def push[T[_[_], _], O[_], V](primitive: PrimitiveHandler[T, O]): Interpreter[T[M, ?], U with V] = {
    val newHead: HandlerStack[T[M, ?]] = HandlerStack.pushFirst(primitive)(this.theMonad)
    val newTail: List[HandlerStack[T[M, ?]]] = this.handlerStacks.map(_.pushNext(primitive))
    val newFailEffectId: EffectId = if (primitive.isFilterable) primitive.effectId else this.failEffectId
    new Interpreter[T[M, ?], U with V](newHead.outerMonad, newHead :: newTail, newFailEffectId)
  }

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
      case Dispatch(id, op) => run(castS(op)(findSig(id)), que)
      case Scope(ux, h) => run(Done(castM(h.prime(push(h.primitive).apply(ux)))), que)
    }
  }

  private def findSig(effectId: EffectId): Signature[U] =
    lookup(effectId).asInstanceOf[Signature[U]]
  
  private def lookup(key: AnyRef): AnyRef = {
    def loop(idx: Int): AnyRef = {
      if (vmt(idx) eq key)
        vmt(idx+1)
      else
        loop(idx+2)
    }
    loop(0)
  }

  private val vmt: Array[AnyRef] = {
    val n = handlerStacks.size
    val arr = new Array[AnyRef]((n + 1) * 2)
    for ((hh, i) <- handlerStacks.iterator.zipWithIndex) {
      arr(i*2) = hh.effectId
      arr(i*2+1) = hh.decoder(this)
    }
    arr(n*2) = null
    arr(n*2+1) = if (failEffectId == null) null else arr(arr.indexOf(failEffectId) + 1)
    arr
  }
}

object Interpreter {
  val pure: Interpreter[Trampoline, Any] = apply(TrampolineInstances.monad)
  val pureStackUnsafe: Interpreter[Id, Any] = apply(MonadPar.identity)
  def apply[M[_]: MonadPar]: Interpreter[M, Any] = new Interpreter[M, Any](MonadPar[M], Nil, null)
}

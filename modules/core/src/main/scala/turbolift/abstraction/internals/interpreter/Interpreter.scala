package turbolift.abstraction.internals.interpreter
import mwords._
import turbolift.abstraction.{!!, ComputationCases}
import turbolift.abstraction.effect.{EffectId, Signature}
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

  private def lookup(effectId: EffectId): Signature[M] = {
    def loop(i: Int): Signature[M] = {
      if (vmt(i) eq effectId)
        vmt(i+1).asInstanceOf[Signature[M]]
      else
        loop(i+2)
    }
    loop(0)
  }

  private val vmt: Array[AnyRef] = {
    val n = handlerStacks.size
    val arr = new Array[AnyRef]((n + 1) * 2)
    for ((hh, i) <- handlerStacks.iterator.zipWithIndex) {
      arr(i*2) = hh.effectId
      arr(i*2+1) = hh.decoder
    }
    arr(n*2) = null
    arr(n*2+1) = if (failEffectId == null) null else arr(arr.indexOf(failEffectId) + 1)
    arr
  }

  override def apply[A](ua: A !! U): M[A] = run(ua, nullMA, Que.empty)

  private def nullMA[A]: M[A] = null.asInstanceOf[M[A]]

  private def run[A](ua: A !! U, ma: M[A], que: Que[M, U]): M[A] = {
    import ComputationCases._
    import QueCases._
    def castS[M1[_], Z[P[_]] <: Signature[P]](f: Z[M1] => M1[A]) = f.asInstanceOf[Signature[M] => M[A]]
    def castU[A1](ua1: A1 !! U) = ua1.asInstanceOf[A !! U]
    def castM[A1](ma1: M[A1]) = ma1.asInstanceOf[M[A]]
    if (ua != null)
      ua match {
        case Pure(a) => run(null, theMonad.pure(a), que)
        case FlatMap(ux, k) => run(castU(ux), nullMA, SeqStep(k, que))
        case ZipPar(ux, uy) => run(castU(ux), nullMA, ParStepLeft(castU(uy), que))
        case DispatchFO(id, op) => run(null, castM(castS(op)(lookup(id))), que)
        case DispatchHO(id, op) => run(null, castM(castS(op(this))(lookup(id))), que)
        case PushHandler(ux, ph) => run(null, castM(ph.prime(push(ph.primitive).apply(ux))), que)
      }
    else
      que match {
        case Empty() => ma
        case SeqStep(f, next) => theMonad.flatMap(ma)(a => run(castU(f(a)), nullMA, next))
        case ParStepLeft(ux, next) => run(castU(ux), nullMA, ParStepRight(ma, next))
        case ParStepRight(mx, next) => run(null, castM(theMonad.zipPar(mx, ma)), next)
      }
  }
}

object Interpreter {
  val pure: Interpreter[Trampoline, Any] = apply(TrampolineInstances.monad)
  val pureStackUnsafe: Interpreter[Identity, Any] = apply(MonadPar.identity)
  def apply[M[_]: MonadPar]: Interpreter[M, Any] = new Interpreter[M, Any](MonadPar[M], Nil, null)
}

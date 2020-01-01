package turbolift.abstraction.handlers
import mwords._
import turbolift.abstraction.handlers.aux.{Trampoline, TrampolineInstances}
import turbolift.abstraction.effect.Signature
import turbolift.abstraction.!!


final class Interpreter[M[_], U](
  val theMonad: MonadPar[M],
  val handlerStacks: List[HandlerStack[M]],
  val failEffectId: AnyRef,
) extends ((? !! U) ~> M) {
  override def apply[A](ua: A !! U): M[A] = loop(ua)

  def push[T[_[_], _], O[_], V](primitive: PrimitiveHandler[T, O]): Interpreter[T[M, ?], U with V] = {
    val newHead: HandlerStack[T[M, ?]] = HandlerStack.pushFirst(primitive)(this.theMonad)
    val newTail: List[HandlerStack[T[M, ?]]] = this.handlerStacks.map(_.pushNext(primitive))
    val newFailEffectId: AnyRef = if (primitive.isFilterable) primitive.effectId else this.failEffectId
    new Interpreter[T[M, ?], U with V](newHead.outerMonad, newHead :: newTail, newFailEffectId)
  }

  private val decoderMap: Map[AnyRef, Signature[M]] = {
    val m: Map[AnyRef, Signature[M]] = handlerStacks.iterator.map(h => h.effectId -> h.decoder).toMap
    val v: Signature[M] = if (failEffectId != null) m(failEffectId) else null
    m + ((null, v))
  }

  private def loop[A](ua: A !! U): M[A] = {
    import turbolift.abstraction.ComputationCases._
    implicit def M: MonadPar[M] = theMonad
    def castM[X](ma: M[X]) = ma.asInstanceOf[M[A]]
    def castS[X[_], Z[P[_]] <: Signature[P]](f: Z[X] => X[A]) = f.asInstanceOf[Signature[M] => M[A]]
    ua match {
      case Pure(a) => theMonad.pure(a)
      case FlatMap(ux, k) => ux match {
        case Pure(x) => loop(k(x))
        case FlatMap(uy, j) => loop(FlatMap(uy, (y: Any) => FlatMap(j(y), k)))
        case _ => loop(ux).flatMap(x => loop(k(x)))
      }
      case ZipPar(uy, uz) => castM(loop(uy) *! loop(uz))
      case DispatchFO(id, op) => castS(op)(decoderMap(id))
      case DispatchHO(id, op) => castS(op(this))(decoderMap(id))
      case HandleInScope(uy, ph) => castM(ph.prime(push(ph.primitive).loop(uy)))
    }
  }
}


object Interpreter {
  val pure: Interpreter[Trampoline, Any] = apply(TrampolineInstances.monad)
  val pureStackUnsafe: Interpreter[Identity, Any] = apply(MonadPar.identity)
  def apply[M[_]: MonadPar]: Interpreter[M, Any] = new Interpreter[M, Any](MonadPar[M], Nil, null)
}

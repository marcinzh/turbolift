package turbolift.abstraction.internals.interpreter
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.Signature
import turbolift.abstraction.internals.handler.PrimitiveHandler


final class Interpreter[M[_], U](
  val theMonad: MonadPar[M],
  val allHandlerStacks: List[HandlerStack[M]],
  val failEffectId: AnyRef,
) extends ((? !! U) ~> M) {
  override def apply[A](ua: A !! U): M[A] = loop(ua)

  def push[T[_[_], _], O[_], V](primitive: PrimitiveHandler[T, O]): Interpreter[T[M, ?], U with V] = {
    val newHead: HandlerStack[T[M, ?]] = HandlerStack.pushFirst(primitive)(this.theMonad)
    val newTail: List[HandlerStack[T[M, ?]]] = this.allHandlerStacks.map(_.pushNext(primitive))
    val newFailEffectId: AnyRef = if (primitive.isFilterable) primitive.effectId else this.failEffectId
    new Interpreter[T[M, ?], U with V](newHead.outerMonad, newHead :: newTail, newFailEffectId)
  }

  private val decoderMap: Map[AnyRef, Signature[M]] = {
<<<<<<< HEAD:modules/core/src/main/scala/turbolift/abstraction/internals/interpreter/Interpreter.scala
    val m: Map[AnyRef, Signature[M]] = handlerStacks.iterator.map(h => h.effectId -> h.decoder).toMap
=======
    val m: Map[AnyRef, Signature[M]] = allHandlerStacks.iterator.map(h => h.effectId -> h.decoder).toMap
>>>>>>> master:modules/core/src/main/scala/turbolift/abstraction/handlers/Interpreter.scala
    val v: Signature[M] = if (failEffectId != null) m(failEffectId) else null
    m + ((null, v))
  }

  private def loop[A](ua: A !! U): M[A] = {
    import turbolift.abstraction.ComputationCases._
    implicit def M: MonadPar[M] = theMonad
<<<<<<< HEAD:modules/core/src/main/scala/turbolift/abstraction/internals/interpreter/Interpreter.scala
    def castM[A1](ma: M[A1]) = ma.asInstanceOf[M[A]]
    def castS[M1[_], Z[P[_]] <: Signature[P]](f: Z[M1] => M1[A]) = f.asInstanceOf[Signature[M] => M[A]]
=======
    def castM[X](ma: M[X]) = ma.asInstanceOf[M[A]]
    def castS[X[_], Z[P[_]] <: Signature[P]](f: Z[X] => X[A]) = f.asInstanceOf[Signature[M] => M[A]]
>>>>>>> master:modules/core/src/main/scala/turbolift/abstraction/handlers/Interpreter.scala
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
<<<<<<< HEAD:modules/core/src/main/scala/turbolift/abstraction/internals/interpreter/Interpreter.scala
      case PushHandler(uy, ph) => castM(ph.prime(push(ph.primitive).loop(uy)))
=======
      case HandleInScope(uy, ph) => castM(ph.prime(push(ph.primitive).loop(uy)))
>>>>>>> master:modules/core/src/main/scala/turbolift/abstraction/handlers/Interpreter.scala
    }
  }
}


object Interpreter {
  val pure: Interpreter[Trampoline, Any] = apply(TrampolineInstances.monad)
  val pureStackUnsafe: Interpreter[Identity, Any] = apply(MonadPar.identity)
  def apply[M[_]: MonadPar]: Interpreter[M, Any] = new Interpreter[M, Any](MonadPar[M], Nil, null)
}

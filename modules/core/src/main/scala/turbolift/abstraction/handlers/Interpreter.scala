package turbolift.abstraction.handlers
import mwords._
import turbolift.abstraction.effect.{Signature, FailSig}
import turbolift.abstraction.!!
import turbolift.abstraction.ComputationCases._
import turbolift.abstraction.handlers.aux.{Trampoline, TrampolineInstances}


sealed trait Interpreter[M[+_], +U] {
  def dispatch[A, Z <: Signature](effectId: AnyRef, op: Z => Z#Op[A]): M[A]
  def dispatchFail: M[Nothing]

  final def push[T[_[+_], +_], V](primitive: PrimitiveHandler[T]): Interpreter[T[M, +?], U with V] =
    new Push[M, U, T, V](primitive: PrimitiveHandler[T], this)

  val theMonad: MonadPar[M]

  final def run_![A](ua: A !! U): M[A] = loop(ua)

  private def loop[A](ua: A !! U): M[A] = {
    implicit def M: MonadPar[M] = theMonad
    ua match {
      case Pure(a) => theMonad.pure(a)
      case FlatMap(ux, k) => ux match {
        case Pure(x) => loop(k(x))
        case FlatMap(uy, j) => loop(FlatMap(uy, (y: Any) => FlatMap(j(y), k)))
        case _ => loop(ux).flatMap(x => loop(k(x)))
      }
      case ZipPar(uy, uz) => loop(uy) *! loop(uz)
      case Dispatch(id, op) => dispatch(id, op)
      case Fail => dispatchFail
      case HandleInScope(uy, ph) =>
        val h2 = push[ph.Trans, ph.Effects](ph.primitive)
        ph.prime(h2.loop(uy))
    }
  }
}


object Interpreter {
  val pure: Interpreter[Trampoline, Any] = apply(TrampolineInstances.monad)
  val pureStackUnsafe: Interpreter[Identity, Any] = apply(MonadPar.identity)

  def apply[M[+_]](implicit M: MonadPar[M]): Interpreter[M, Any] = new Interpreter[M, Any] {
    override val theMonad: MonadPar[M] = M
    override def dispatch[A, Z <: Signature](effectId: AnyRef, op: Z => Z#Op[A]): M[A] = unhandled(effectId.toString)
    override def dispatchFail: M[Nothing] = unhandled("Fail")
    def unhandled(msg: String) = sys.error(s"Unhandled effect: $msg")
  }
}


private[abstraction] final class Push[M[+_], U, T[_[+_], +_], V](
  val primitive: PrimitiveHandler[T],
  val next: Interpreter[M, U]
) extends Interpreter[T[M, +?], U with V] {
  type TM[+A] = T[M, A]

  private val commonOps = primitive.commonOps[M]
  private val specialOps = primitive.specialOps[M]
  
  implicit def nextMonad: MonadPar[M] = next.theMonad
  override val theMonad: MonadPar[TM] = commonOps

  override def dispatch[A, Z <: Signature](effectId: AnyRef, op: Z => Z#Op[A]): TM[A] =
    if (effectId eq primitive.effectId)
      op.asInstanceOf[primitive.Encoded[M, A]](specialOps)
    else
      commonOps.lift(next.dispatch(effectId, op))

  override def dispatchFail: TM[Nothing] =
    if(primitive.isFilterable)
      FailSig.encodeFail.asInstanceOf[primitive.Encoded[M, Nothing]](specialOps)
    else
      commonOps.lift(next.dispatchFail)
}

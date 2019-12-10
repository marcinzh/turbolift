package turbolift.abstraction.handlers
import mwords._
import turbolift.abstraction.effect.{Signature, FailSig}
import turbolift.abstraction.!!
import turbolift.abstraction.ComputationCases._


sealed trait HandlerStack[M[+_], +U] extends MonadPar[M] {
  def dispatch[A, Z <: Signature](effectId: AnyRef, op: Z => Z#Op[A]): M[A]
  def dispatchFail: M[Nothing]

  final def push[T[_[+_], +_], V](ih: ImpureHandler[T]): HandlerStack[T[M, +?], U with V] =
    new StackedHandler[M, U, T, V](ih: ImpureHandler[T], this)

  implicit def theMonad: MonadPar[M] = this

  final def run_![A](ua: A !! U): M[A] = loop(ua)

  private def loop[A](ua: A !! U): M[A] =
    ua match {
      case Pure(a) => pure(a)
      case FlatMap(ux, k) => ux match {
        case Pure(x) => loop(k(x))
        case FlatMap(uy, j) => loop(FlatMap(uy, (y: Any) => FlatMap(j(y), k)))
        case _ => loop(ux).flatMap(x => loop(k(x)))
      }
      case ZipPar(uy, uz) => zipPar(loop(uy), loop(uz))
      case Dispatch(id, op) => dispatch(id, op)
      case Fail => dispatchFail
      case HandleInScope(uy, ph) =>
        val h2 = push[ph.Trans, ph.Effects](ph.impure)
        ph.gimmick(h2.loop(uy))
  }
}

object HandlerStack {
  val pure: HandlerStack[Trampoline, Any] = apply(TrampolineInstances.monad)
  val pureStackUnsafe: HandlerStack[Identity, Any] = apply(MonadPar.identity)

  def apply[M[+_]](implicit M: MonadPar[M]): HandlerStack[M, Any] = new Unhandled[M] {
    def pure[A](a: A): M[A] = M.pure(a)
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = M.flatMap(ma)(f)
    def zipPar[A, B](ma: M[A], mb: M[B]): M[(A, B)] = M.zipPar(ma, mb)
  }

  trait Unhandled[M[+_]] extends HandlerStack[M, Any] {
    def dispatch[A, Z <: Signature](effectId: AnyRef, op: Z => Z#Op[A]): M[A] = unhandled(effectId.toString)
    def dispatchFail: M[Nothing] = unhandled("Fail")
    private def unhandled(msg: String) = sys.error(s"Unhandled effect: $msg")
  }
}


private[abstraction] final class StackedHandler[M[+_], U, T[_[+_], +_], V](
  val impure: ImpureHandler[T],
  val next: HandlerStack[M, U]
) extends HandlerStack[T[M, +?], U with V] {
  private implicit def M: MonadPar[M] = next
  private type TM[+A] = T[M, A]

  def pure[A](a: A): TM[A] = impure.lift(next.pure(a))
  def flatMap[A, B](ma: TM[A])(f: A => TM[B]): TM[B] = impure.flatMap(ma)(f)
  def zipPar[A, B](ma: TM[A], mb: TM[B]): TM[(A, B)] = impure.zipPar(ma, mb)

  private val decode = impure.decode[M]

  def dispatch[A, Z <: Signature](effectId: AnyRef, op: Z => Z#Op[A]): TM[A] =
    if (effectId eq impure.effectId)
      op.asInstanceOf[impure.Encoded[M, A]](decode)
    else
      impure.lift(next.dispatch(effectId, op))

  def dispatchFail: TM[Nothing] =
    if(impure.isFilterable)
      FailSig.encodeFail.asInstanceOf[impure.Encoded[M, Nothing]](decode)
    else
      impure.lift(next.dispatchFail)
}

package turbolift.abstraction.handlers
import mwords._
import turbolift.abstraction.effect.{Signature, FailSig}


sealed trait HandlerStack[M[+_], +U] extends MonadPar[M] {
  def dispatch[A, Z <: Signature](effectId: AnyRef, op: Z => Z#Op[A]): M[A]
  def dispatchFail: M[Nothing]

  final def push[T[_[+_], +_], V](ih: ImpureHandler[T]): HandlerStack[T[M, +?], U with V] =
    new StackedHandler[M, U, T, V](ih: ImpureHandler[T], this)
}

object HandlerStack {
  val pure: HandlerStack[Identity, Any] = new Unhandled[Identity] {
    def pure[A](a: A): A = a
    def flatMap[A, B](a: A)(f: A => B): B = f(a)
    def zipPar[A, B](a: A, b: B): (A, B) = (a, b)
  }

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
) extends HandlerStack[
  T[M, +?],
  U with V
] {
  private implicit def M: MonadPar[M] = next
  private type TM[+A] = T[M, A]

  def pure[A](a: A): TM[A] = impure.lift(next.pure(a))
  def flatMap[A, B](ma: TM[A])(f: A => TM[B]): TM[B] = impure.flatMap(ma)(f)
  def zipPar[A, B](ma: TM[A], mb: TM[B]): TM[(A, B)] = impure.zipPar(ma, mb)

  private val decode = impure.decode[M]
  def dispatch[A, Z <: Signature](effectId: AnyRef, op: Z => Z#Op[A]): TM[A] =
    if (effectId eq impure.effectId)
      op.asInstanceOf[impure.Decode[M] => impure.Decode[M]#Op[A]](decode)
    else
      impure.lift(next.dispatch(effectId, op))

  def dispatchFail: TM[Nothing] =
    if(impure.isFilterable)
      FailSig.encodeFail.asInstanceOf[impure.Decode[M] => impure.Decode[M]#Op[Nothing]](decode)
    else
      impure.lift(next.dispatchFail)

    
  //@#@ [M]
  // def dispatchFail: TM[Nothing] =
  // 	impure.empty_?[M] match {
  // 		case Some(tma) => tma
  // 		case None => impure.lift(next.dispatchFail)
  // 	}
}

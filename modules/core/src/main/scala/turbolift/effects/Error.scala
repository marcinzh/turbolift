package turbolift.effects
import scala.util.{Try, Success, Failure}
import turbolift.{!!, Effect, Signature}
import turbolift.typeclass.{Accum, One}
import turbolift.effects.default_handlers.{errorHandler_first, errorHandler_all} 


trait ErrorSignature[E, E1] extends Signature:
  def raise(e: E1): Nothing !@! ThisEffect
  def raises(e: E): Nothing !@! ThisEffect
  def catchAll[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !@! U


trait ErrorEffect[E, E1] extends Effect[ErrorSignature[E, E1]] with ErrorSignature[E, E1]:
  final override def raise(e: E1): Nothing !! this.type = perform(_.raise(e))
  final override def raises(e: E): Nothing !! this.type = perform(_.raises(e))
  final override def catchAll[A, U <: this.type](body: A !! U)(f: E => A !! U): A !! U = perform(_.catchAll(body)(f))

  final def raise[K, V1](k: K, v: V1)(using ev: ((K, V1)) <:< E1): Unit !! this.type = raise(ev((k, v)))
  final def raises[K, V](k: K, v: V)(using ev: ((K, V)) <:< E): Unit !! this.type = raises(ev((k, v)))

  final def fromOption[A](x: Option[A])(e: => E1): A !! this.type = x.fold(raise(e))(pure)
  final def fromEither[A](x: Either[E1, A]): A !! this.type = x.fold(raise, pure)
  final def fromTry[A](x: Try[A])(using ev: Throwable <:< E1): A !! this.type = x.fold(e => raise(ev(e)), pure)

  /** Default handler for this effect. */
  final def handler(using E: One[E, E1]): ThisHandler.Free[Either[E, _]] = handlers.first

  /** Predefined handlers for this effect. */
  object handlers:
    def first(using One[E, E1]): ThisHandler.Free[Either[E, _]] = ErrorEffect.this.errorHandler_first
    def all(using Accum[E, E1]): ThisHandler.Free[Either[E, _]] = ErrorEffect.this.errorHandler_all


trait Error[E] extends ErrorEffect[E, E]

trait ErrorK[F[_], E] extends ErrorEffect[F[E], E]

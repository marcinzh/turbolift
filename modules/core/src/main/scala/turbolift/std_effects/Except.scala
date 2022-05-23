package turbolift.std_effects
import scala.util.{Try, Success, Failure}
import turbolift.{!!, Effect, Signature}
import turbolift.typeclass.Accum
import turbolift.std_effects.default_handlers.{ExceptHandler_One, ExceptHandler_Many} 


trait ExceptSig[E, E1] extends Signature:
  def raise(e: E1): Nothing !@! ThisEffect
  def raises(e: E): Nothing !@! ThisEffect
  def katch[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !@! U


trait ExceptEffect[E, E1] extends Effect[ExceptSig[E, E1]] with ExceptSig[E, E1]:
  final override def raise(e: E1): Nothing !! this.type = perform(_.raise(e))
  final override def raises(e: E): Nothing !! this.type = perform(_.raises(e))
  final override def katch[A, U <: this.type](body: A !! U)(f: E => A !! U): A !! U = perform(_.katch(body)(f))

  final def raise[K, V1](k: K, v: V1)(implicit ev: ((K, V1)) <:< E1): Unit !! this.type = raise(ev((k, v)))
  final def raises[K, V](k: K, v: V)(implicit ev: ((K, V)) <:< E): Unit !! this.type = raises(ev((k, v)))

  final def fromOption[A](x: Option[A])(e: => E1): A !! this.type = x.fold(raise(e))(pure)
  final def fromEither[A](x: Either[E1, A]): A !! this.type = x.fold(raise, pure)
  final def fromTry[A](x: Try[A])(implicit ev: Throwable <:< E1): A !! this.type = x.fold(e => raise(ev(e)), pure)

  final def handler(implicit E: E1 =:= E): ThisHandler.Free[Either[E, _]] = handlers.one

  object handlers:
    def one(implicit E: E1 =:= E): ThisHandler.Free[Either[E, _]] = ExceptHandler_One[E, E1, ExceptEffect.this.type](ExceptEffect.this)
    def many(implicit E: Accum[E, E1]): ThisHandler.Free[Either[E, _]] = ExceptHandler_Many[E, E1, ExceptEffect.this.type](ExceptEffect.this)


trait Except[E] extends ExceptEffect[E, E]

trait ExceptK[F[_], E] extends ExceptEffect[F[E], E]

trait ExceptG[M[_, _], K, V] extends ExceptEffect[M[K, V], (K, V)]

trait ExceptGK[M[_, _], K, F[_], V] extends ExceptEffect[M[K, F[V]], (K, V)]

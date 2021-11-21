package turbolift.std_effects
import scala.util.{Try, Success, Failure}
import turbolift.abstraction.{!!, Effect, Signature}
import turbolift.abstraction.typeclass.Accum


trait ExceptExtSig[E, E1] extends Signature:
  def raise(e: E1): Nothing !@! ThisEffect
  def raises(e: E): Nothing !@! ThisEffect
  def katch[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !@! U


trait ExceptExt[E, E1] extends Effect[ExceptExtSig[E, E1]] with ExceptExtSig[E, E1]:
  final override def raise(e: E1): Nothing !! this.type = impure(_.raise(e))
  final override def raises(e: E): Nothing !! this.type = impure(_.raises(e))
  final override def katch[A, U <: this.type](body: A !! U)(f: E => A !! U): A !! U = impure(_.katch(body)(f))

  final def raise[K, V1](k: K, v: V1)(implicit ev: ((K, V1)) <:< E1): Unit !! this.type = raise(ev((k, v)))
  final def raises[K, V](k: K, v: V)(implicit ev: ((K, V)) <:< E): Unit !! this.type = raises(ev((k, v)))

  final def fromOption[A](x: Option[A])(e: => E1): A !! this.type = x.fold(raise(e))(pure)
  final def fromEither[A](x: Either[E1, A]): A !! this.type = x.fold(raise, pure)
  final def fromTry[A](x: Try[A])(implicit ev: Throwable <:< E1): A !! this.type = x.fold(e => raise(ev(e)), pure)

  object handlers:
    def one(implicit E: E1 =:= E): ThisIHandler[Either[E, _]] = ExceptHandler_One[E, E1, ExceptExt.this.type](ExceptExt.this)
    def many(implicit E: Accum[E, E1]): ThisIHandler[Either[E, _]] = ExceptHandler_Many[E, E1, ExceptExt.this.type](ExceptExt.this)



object ExceptExt:
  trait One[E, E1] extends ExceptExt[E, E1]:
    def handler(implicit E: E1 =:= E): ThisIHandler[Either[E, _]] = handlers.one

  trait Many[E, E1] extends ExceptExt[E, E1]:
    def handler(implicit E: Accum[E, E1]): ThisIHandler[Either[E, _]] = handlers.many


trait Except[E] extends ExceptExt.One[E, E]

trait ExceptK[F[_], E] extends ExceptExt.One[F[E], E]

trait ExceptG[M[_, _], K, V] extends ExceptExt.One[M[K, V], (K, V)]

trait ExceptGK[M[_, _], K, F[_], V] extends ExceptExt.One[M[K, F[V]], (K, V)]

trait Validation[E] extends ExceptExt.Many[E, E]

trait ValidationK[F[_], E] extends ExceptExt.Many[F[E], E]

trait ValidationG[M[_, _], K, V] extends ExceptExt.Many[M[K, V], (K, V)]

trait ValidationGK[M[_, _], K, F[_], V] extends ExceptExt.Many[M[K, F[V]], (K, V)]

type ExceptSig[E] = ExceptExtSig[E, E]

type ExceptKSig[F[_], E] = ExceptExtSig[F[E], E]

type ExceptGSig[M[_, _], K, V] = ExceptExtSig[M[K, V], (K, V)]

type ExceptGKSig[M[_, _], K, F[_], V] = ExceptExtSig[M[K, F[V]], (K, V)]

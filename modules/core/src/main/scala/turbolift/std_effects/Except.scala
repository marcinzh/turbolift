package turbolift.std_effects
import scala.util.{Try, Success, Failure}
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.Accum


trait ExceptExtSig[U, E, E1]:
  def raise[A](e: E1): A !! U
  def raises[A](e: E): A !! U
  def katch[A](body: A !! U)(fun: E => A !! U): A !! U


trait ExceptExt[E, E1] extends Effect[ExceptExtSig[_, E, E1]]:
  final def raise(e: E1): Nothing !! this.type = impureFO(_.raise(e))
  final def raises(e: E): Nothing !! this.type = impureFO(_.raises(e))
  final def raise[K, V1](k: K, v: V1)(implicit ev: ((K, V1)) <:< E1): Unit !! this.type = raise(ev((k, v)))
  final def raises[K, V](k: K, v: V)(implicit ev: ((K, V)) <:< E): Unit !! this.type = raises(ev((k, v)))
  final def katch[A, U <: this.type](body: A !! U)(fun: E => A !! U): A !! U = impureHO[U](_.katch(body)(fun))

  final def fromEither[A](x: Either[E1, A]): A !! this.type =
    x match
      case Right(a) => pure(a)
      case Left(e) => raise(e)

  final def fromOption[A](x: Option[A])(e : => E1): A !! this.type =
    x match
      case Some(a) => pure(a)
      case _ => raise(e)

  final def fromTry[A](x: Try[A])(implicit ev: E1 <:< Throwable): A !! this.type =
    x match
      case Success(a) => pure(a)
      case Failure(e) => raise(e.asInstanceOf[E1])

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

type ExceptSig[U, E] = ExceptExtSig[U, E, E]

type ExceptKSig[U, F[_], E] = ExceptExtSig[U, F[E], E]

type ExceptGSig[U, M[_, _], K, V] = ExceptExtSig[U, M[K, V], (K, V)]

type ExceptGKSig[U, M[_, _], K, F[_], V] = ExceptExtSig[U, M[K, F[V]], (K, V)]

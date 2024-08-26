package turbolift.effects
import scala.util.{Try, Success, Failure}
import turbolift.{!!, Effect, Signature}
import turbolift.typeclass.{Accum, One}
import turbolift.handlers.{errorHandler_first, errorHandler_all} 


trait ErrorSignature[E, E1] extends Signature:
  def raise(e: E1): Nothing !! ThisEffect
  def raises(e: E): Nothing !! ThisEffect
  def toEither[A, U <: ThisEffect](body: A !! U): Either[E, A] !! U
  def catchAllEff[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !! U


trait ErrorEffect[E, E1] extends Effect[ErrorSignature[E, E1]] with ErrorSignature[E, E1]:
  final override def raise(e: E1): Nothing !! this.type = perform(_.raise(e))
  final override def raises(e: E): Nothing !! this.type = perform(_.raises(e))
  final override def toEither[A, U <: this.type](body: A !! U): Either[E, A] !! U = perform(_.toEither(body))
  final override def catchAllEff[A, U <: this.type](body: A !! U)(f: E => A !! U): A !! U = perform(_.catchAllEff(body)(f))

  final def catchAll[A, U <: this.type](body: A !! U)(f: E => A): A !! U = catchAllEff(body)(f.andThen(!!.pure))
  final def catchSome[A, U <: this.type](body: A !! U)(f: PartialFunction[E, A]): A !! U = catchSomeEff(body)(f.andThen(!!.pure))
  final def catchSomeEff[A, U <: this.type](body: A !! U)(f: PartialFunction[E, A !! U]): A !! U = catchAllEff(body)(f.applyOrElse(_, raises))

  final def raise[K, V1](k: K, v: V1)(using ev: ((K, V1)) <:< E1): Unit !! this.type = raise(ev((k, v)))
  final def raises[K, V](k: K, v: V)(using ev: ((K, V)) <:< E): Unit !! this.type = raises(ev((k, v)))

  final def fromOption[A](x: Option[A])(e: => E1): A !! this.type = x.fold(raise(e))(!!.pure)
  final def fromEither[A](x: Either[E1, A]): A !! this.type = x.fold(raise, !!.pure)
  final def fromTry[A](x: Try[A])(using ev: Throwable <:< E1): A !! this.type = x.fold(e => raise(ev(e)), !!.pure)

  /** Predefined handlers for this effect. */
  object handlers:
    def first(using One[E, E1]): ThisHandler[Identity, Either[E, _], Any] = ErrorEffect.this.errorHandler_first
    def all(using Accum[E, E1]): ThisHandler[Identity, Either[E, _], Any] = ErrorEffect.this.errorHandler_all
    def allReduce(using E =:= E1)(plus: (E, E1) => E): ThisHandler[Identity, Either[E, _], Any] = all(using Accum.instanceEq( plus))


trait Error[E] extends ErrorEffect[E, E]:
  export handlers.{first => handler}

trait ErrorK[F[_], E] extends ErrorEffect[F[E], E]:
  export handlers.{first => handler}

trait ErrorG[M[_, _], K, V] extends ErrorEffect[M[K, V], (K, V)]:
  export handlers.{first => handler}

trait ErrorGK[M[_, _], K, F[_], V] extends ErrorEffect[M[K, F[V]], (K, V)]:
  export handlers.{first => handler}

package turbolift.effects
import scala.util.{Try, Success, Failure}
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.typeclass.{Accum, One}
import turbolift.typeclass.Syntax._


/** Signature of [[ErrorEffectExt]].
 *
 * @tparam E Accumulated error
 * @tparam E1 Singular error added to the accumulator (commonly same as [[E]])
 */
trait ErrorSignature[E, E1] extends Signature:
  def raise(e: E1): Nothing !! ThisEffect
  def raises(e: E): Nothing !! ThisEffect
  def catchToEither[A, U <: ThisEffect](body: A !! U): Either[E, A] !! U

  @deprecated("Use catchToEither") final def toEither[A, U <: ThisEffect](body: A !! U): Either[E, A] !! U = catchToEither(body)


/** Base trait for custom instances of Error effect.
 *
 * {{{
 * case object MyError extends ErrorEffect[String]
 * // optional:
 * type MyError = MyError.type
 * }}}
 *
 * This effect covers both applicative-error and monadic-error.
 * The behavior depends on:
 * - selected handler: `first` vs. `all`
 * - composition of computations: sequential (`flatMap`, `zip`, etc.) vs. parallel (`zipPar`, etc.).
 *
 * Notice that [[ErrorEffectExt]] takes type 2 parameters.
 * This abstraction enables ergonomic syntax of `raise(e)`,
 * which relieves the user from the necessity of wrapping `e` in `Nel` (`raise(Nel(e))`),
 * as in the standard `Validated` applicative functor.
 * For the simpler, single-parmeter version, see [[ErrorEffect]].
 *
 * @see [[ErrorEffect]]
 * @see [[ErrorEffectK]]
 * @see [[ErrorEffectG]]
 * @see [[ErrorEffectGK]]
 * @see [[PolyErrorEffect]]
 * @see [[Error]]
 *
 * @tparam E Accumulated error
 * @tparam E1 Singular error added to the accumulator (commonly same as [[E]])
 */
trait ErrorEffectExt[E, E1] extends Effect[ErrorSignature[E, E1]] with ErrorSignature[E, E1]:
  enclosing =>
  final override def raise(e: E1): Nothing !! this.type = perform(_.raise(e))
  final override def raises(e: E): Nothing !! this.type = perform(_.raises(e))
  final override def catchToEither[A, U <: this.type](body: A !! U): Either[E, A] !! U = perform(_.catchToEither(body))

  final def catchAll[A, U <: this.type](body: A !! U)(f: E => A): A !! U = catchAllEff(body)(f.andThen(!!.pure))
  final def catchAllEff[A, U <: this.type](body: A !! U)(f: E => A !! U): A !! U = catchToEither(body).flatMap(_.fold(f, !!.pure))
  final def catchSome[A, U <: this.type](body: A !! U)(f: PartialFunction[E, A]): A !! U = catchSomeEff(body)(f.andThen(!!.pure))
  final def catchSomeEff[A, U <: this.type](body: A !! U)(f: PartialFunction[E, A !! U]): A !! U = catchAllEff(body)(f.applyOrElse(_, raises))

  final def raise[K, V1](k: K, v: V1)(using ev: ((K, V1)) <:< E1): Unit !! this.type = raise(ev((k, v)))
  final def raises[K, V](k: K, v: V)(using ev: ((K, V)) <:< E): Unit !! this.type = raises(ev((k, v)))

  final def raiseFromOption[A](x: Option[A])(e: => E1): A !! this.type = x.fold(raise(e))(!!.pure)
  final def raiseFromEither[A](x: Either[E1, A]): A !! this.type = x.fold(raise, !!.pure)
  final def raiseFromTry[A](x: Try[A])(using ev: Throwable <:< E1): A !! this.type = x.fold(e => raise(ev(e)), !!.pure)

  @deprecated("Use raiseFromOption") final def fromOption[A](x: Option[A])(e: => E1): A !! this.type = raiseFromOption(x)(e)
  @deprecated("Use raiseFromEither") final def fromEither[A](x: Either[E1, A]): A !! this.type = raiseFromEither(x)
  @deprecated("Use raiseFromTry") final def fromTry[A](x: Try[A])(using ev: Throwable <:< E1): A !! this.type = raiseFromTry(x)

  /** Predefined handlers for this effect. */
  object handlers:
    /** Short-circuit on the first error. */
    def first(using E: One[E, E1]): Handler[Identity, Either[E, _], enclosing.type, Any] =
      new impl.Stateless[Identity, Either[E, _], Any] with impl.Sequential.Restartable with ErrorSignature[E, E1]:
        override def onReturn(a: Unknown): Either[E, Unknown] !! Any = !!.pure(Right(a))
        override def onRestart(aa: Either[E, Unknown]): Unknown !! enclosing.type = aa.fold(enclosing.raises, !!.pure)
        override def onOnce(aa: Either[E, Unknown]): Option[Unknown] = aa.toOption

        override def raise(e: E1): Nothing !! ThisEffect = raises(E.one(e))
        override def raises(e: E): Nothing !! ThisEffect = Control.abort(Left(e))
        override def catchToEither[A, U <: ThisEffect](body: A !! U): Either[E, A] !! U = Control.delimit(body)
      .toHandler

    /** Accumulate all errors (Applicative). */
    def all(using E: Accum[E, E1]): Handler[Identity, Either[E, _], enclosing.type, Any] =
      new impl.Stateless[Identity, Either[E, _], Any] with impl.Parallel with ErrorSignature[E, E1]:
        override def onReturn(a: Unknown): Either[E, Unknown] !! Any = !!.pure(Right(a))
        override def onRestart(aa: Either[E, Unknown]): Unknown !! enclosing.type = aa.fold(enclosing.raises, !!.pure)
        override def onOnce(aa: Either[E, Unknown]): Option[Unknown] = aa.toOption
        override def onZip[A, B, C](ea: Either[E, A], eb: Either[E, B], k: (A, B) => C): Either[E, C] =
          (ea, eb) match
            case (Right(a), Right(b)) => Right(k(a, b))
            case (Left(e1), Left(e2)) => Left(e1 |+| e2)
            case (Left(e), _) => Left(e)
            case (_, Left(e)) => Left(e)

        override def raise(e: E1): Nothing !! ThisEffect = raises(E.one(e))
        override def raises(e: E): Nothing !! ThisEffect = Control.abort(Left(e))
        override def catchToEither[A, U <: ThisEffect](body: A !! U): Either[E, A] !! U = Control.delimit(body)
      .toHandler

    /** Lile [[all]], but accumulate with given function instead of typeclass. */
    def allReduce(using E =:= E1)(plus: (E, E1) => E): Handler[Identity, Either[E, _], enclosing.type, Any] =
      all(using Accum.instanceEq( plus))


object ErrorEffectExt:
  extension [E, E1](thiz: ErrorEffectExt[E, E1])
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler(using E: One[E, E1]): Handler[Identity, Either[E, _], thiz.type, Any] = thiz.handlers.first


/** Specialized [[ErrorEffectExt]], where `E` and `E1` are the same (e.g. a semigroup). */
trait ErrorEffect[E] extends ErrorEffectExt[E, E]

/** Specialized [[ErrorEffectExt]], where errors `E` are accumulated into `F[E]` (e.g. a collection). */
trait ErrorEffectK[F[_], E] extends ErrorEffectExt[F[E], E]

/** Specialized [[ErrorEffectExt]], where pairs `(K, V)` are accumulated into `M[K, V]` (e.g. a map). */
trait ErrorEffectG[M[_, _], K, V] extends ErrorEffectExt[M[K, V], (K, V)]

/** Specialized [[ErrorEffectExt]], where pairs `(K, V)` are accumulated into `M[K, F[V]]` (e.g. a map of collections). */
trait ErrorEffectGK[M[_, _], K, F[_], V] extends ErrorEffectExt[M[K, F[V]], (K, V)]


/** Polymorphic variant of [[ErrorEffect]].
 *
 * In the monomorphic variant, the `E` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * // The `E` is explicitly set as `String`:
 * case object MyError extends ErrorEffect[String]
 *
 * // The `E` is inferred from the effect instance:
 * val computation = Myerror.raise("OMG")
 * }}}
 *
 * In the polymorphic variant, the `E` type parameter is **contravariantly** inferred
 * at call sites of effect's operations and handlers.
 * In practice, the type can "grow as you go":
 *
 * {{{
 * case object MyError extends PolyErrorEffect
 *
 * val computation1 = MyError.raise(42)              // `E` inferred as `Int`
 * val computation2 = MyError.raise("OMG")           // `E` inferred as `String`
 * val computation3 = computation1 &&! computation2  // `E` inferred as `Int | String`
 *
 * // Inferred types of the above computations:
 * val _: Nothing !! MyError.@@[Int]          = computation1
 * val _: Nothing !! MyError.@@[String]       = computation2
 * val _: Nothing !! MyError.@@[Int | String] = computation3
 * }}}
 */
abstract class PolyErrorEffect extends Effect.Polymorphic_-[ErrorEffect, Any](new ErrorEffect[Any] {}):
  final def raise[E](e: E): Nothing !! @@[E] = polymorphize[E].perform(_.raise(e))
  final def catchToEither[E] = new CatchToEitherApply[E]
  final def catchAll[E] = new CatchAllApply[E]
  final def catchAllEff[E] = new CatchAllEffApply[E]
  final def catchSome[E] = new CatchSomeApply[E]
  final def catchSomeEff[E] = new CatchSomeEffApply[E]

  final def fromOption[A, E](x: Option[A])(e: => E): A !! @@[E] = x.fold(raise(e))(!!.pure)
  final def fromEither[A, E](x: Either[E, A]): A !! @@[E] = x.fold(raise, !!.pure)
  final def fromTry[A, E](x: Try[A])(using ev: Throwable <:< E): A !! @@[E] = x.fold(e => raise(ev(e)), !!.pure)

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CatchToEitherApply[E]:
    def apply[A, U <: @@[E]](body: A !! U): Either[E, A] !! U = polymorphize[E].perform(_.catchToEither(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CatchAllApply[E]:
    def apply[A, U <: @@[E]](body: A !! U)(f: E => A): A !! U = polymorphize[E].perform(_.catchAll(body)(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CatchAllEffApply[E]:
    def apply[A, U <: @@[E]](body: A !! U)(f: E => A !! U): A !! U = polymorphize[E].perform(_.catchAllEff(body)(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CatchSomeApply[E]:
    def apply[A, U <: @@[E]](body: A !! U)(f: PartialFunction[E, A]): A !! U = polymorphize[E].perform(_.catchSome(body)(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CatchSomeEffApply[E]:
    def apply[A, U <: @@[E]](body: A !! U)(f: PartialFunction[E, A !! U]): A !! U = polymorphize[E].perform(_.catchSomeEff(body)(f))


  /** Predefined handlers for this effect. */
  object handlers:
    def default[E]: Handler[Identity, Either[E, _], @@[E], Any] = polymorphize[E].handler(_.handlers.first)


object PolyErrorEffect:
  extension (thiz: PolyErrorEffect)
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler[E]: Handler[Identity, Either[E, _], thiz.@@[E], Any] = thiz.handlers.default

/** Predefined instance of [[PolyErrorEffect]].
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object Error extends PolyErrorEffect
type Error[E] = Error.@@[E]

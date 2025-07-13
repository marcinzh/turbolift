package turbolift.effects
import scala.util.{Try, Success, Failure}
import turbolift.{!!, Effect, Handler}
import turbolift.Extensions._


/** Polymorphic variant of [[ErrorEffect]].
 *
 * In the monomorphic variant, the `E` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * case object MyError extends ErrorEffect[String]   // `E` supplied as `Int`
 *
 * val computation = Myerror.raise("OMG")  // `E` inferred from the effect instance
 * }}}
 *
 * In the polymorphic variant, the `E` type parameter is **contravariantly** inferred
 * at call sites of effect's operations and handlers:
 *
 * {{{
 * case object MyError extends PolyErrorEffect
 *
 * val computation1 = MyError.raise("OMG")           // `E` inferred as `String`
 * val computation2 = MyError.raise(42)              // `E` inferred as `Int`
 * val computation3 = computation1 &&! computation2  // `E` inferred as `Int | String`
 * }}}
 */
abstract class PolyErrorEffect extends Effect.Polymorphic_-[[X] =>> ErrorEffect[X, X], Any](new ErrorEffect[Any, Any] {}):
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
    def apply[A, U <: @@[E]](body: A !! U)(f: E => A): A !! U = catchAllEff(body)(f.andThen(!!.pure))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CatchAllEffApply[E]:
    def apply[A, U <: @@[E]](body: A !! U)(f: E => A !! U): A !! U = catchToEither(body).flatMap(_.fold(f, !!.pure))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CatchSomeApply[E]:
    def apply[A, U <: @@[E]](body: A !! U)(f: PartialFunction[E, A]): A !! U = catchSomeEff(body)(f.andThen(!!.pure))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CatchSomeEffApply[E]:
    def apply[A, U <: @@[E]](body: A !! U)(f: PartialFunction[E, A !! U]): A !! U = catchAllEff(body)(f.applyOrElse(_, raise))


  /** Predefined handlers for this effect. */
  object handlers:
    def default[E]: Handler[Identity, Either[E, _], @@[E], Any] = polymorphize[E].handler(_.handlers.first)


/** Predefined instance of [[PolyError]] effect.
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object PolyError extends PolyErrorEffect

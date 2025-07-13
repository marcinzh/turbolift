package turbolift.effects
import turbolift.{!!, Effect, Handler}
import turbolift.Extensions._


/** Polymorphic variant of [[ReaderEffect]].
 *
 * In the monomorphic variant, the `R` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * case object MyReader extends ReaderEffect[String]  // `R` supplied as `String`
 *
 * val computation = MyReader.ask   // `R` inferred from the effect instance
 * }}}
 *
 * In the polymorphic variant, the `R` type parameter is **covariantly** inferred
 * at call sites of effect's operations and handlers:
 *
 * {{{
 * case object MyReader extends PolyReaderEffect
 *
 * val computation1 = MyReader.ask[String]           // `R` inferred as `String`
 * val computation2 = MyReader.ask[Int]              // `R` inferred as `Int`
 * val computation3 = computation1 &&! computation2  // `R` inferred as `Int & String`
 * }}}
 */
abstract class PolyReaderEffect extends Effect.Polymorphic_+(new ReaderEffect[Any] {}):
  final def ask[R]: R !! @@[R] = polymorphize[R].perform(_.ask)
  final def asks[R] = new AsksApply[R]
  final def asksEff[R] = new AsksEffApply[R]
  final def localPut[R] = new LocalPutApply[R]
  final def localPutEff[R] = new LocalPutEffApply[R]
  final def localModify[R] = new LocalModifyApply[R]
  final def localModifyEff[R] = new LocalModifyEffApply[R]

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class AsksApply[R]:
    def apply[A](f: R => A): A !! @@[R] = polymorphize[R].perform(_.asks(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class AsksEffApply[R]:
    def apply[A, U <: @@[R]](f: R => A !! U): A !! U = polymorphize[R].perform(_.asksEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class LocalPutApply[R]:
    def apply[A, U <: @@[R]](r: R)(body: A !! U): A !! U = polymorphize[R].perform(_.localPut(r)(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class LocalPutEffApply[R]:
    def apply[A, U <: @@[R]](r: R !! U)(body: A !! U): A !! U = polymorphize[R].perform(_.localPutEff(r)(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class LocalModifyApply[R]:
    def apply[A, U <: @@[R]](f: R => R)(body: A !! U): A !! U = polymorphize[R].perform(_.localModify(f)(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class LocalModifyEffApply[R]:
    def apply[A, U <: @@[R]](f: R => R !! U)(body: A !! U): A !! U = polymorphize[R].perform(_.localModifyEff(f)(body))


  /** Predefined handlers for this effect. */
  object handlers:
    def default[R](initial: R): Handler[Identity, Identity, @@[R], Any] =
      polymorphize[R].handler(_.handlers.default(initial))


/** Predefined instance of [[PolyReader]] effect.
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object PolyReader extends PolyReaderEffect

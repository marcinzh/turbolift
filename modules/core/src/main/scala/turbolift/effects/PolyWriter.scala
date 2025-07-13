package turbolift.effects
import turbolift.{!!, Effect, Handler}
import turbolift.Extensions._
import turbolift.typeclass.PlusZero


/** Polymorphic variant of [[WriterEffect]].
 *
 * In the monomorphic variant, the `W` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * case object MyWriter extends WriterEffect[Int]  // `W` supplied as `Int`
 *
 * val computation = MyWriter.tell(42)  // `W` inferred from the effect instance
 * }}}
 *
 * In the polymorphic variant, the `W` type parameter is **contravariantly** inferred
 * at call sites of effect's operations and handlers:
 *
 * {{{
 * case object MyWriter extends PolyWriterEffect
 *
 * val computation1 = MyWriter.tell("OMG")           // `W` inferred as `String`
 * val computation2 = MyWriter.tell(42)              // `W` inferred as `Int`
 * val computation3 = computation1 &&! computation2  // `W` inferred as `Int | String`
 * }}}
 */
abstract class PolyWriterEffect extends Effect.Polymorphic_-[[X] =>> WriterEffect[X, X], Any](new WriterEffect[Any, Any] {}):
  final def tell[W](w: W): Unit !! @@[W] = polymorphize[W].perform(_.tell(w))
  final def mute[W] = MuteApply[W]
  final def listen[W] = ListenApply[W]
  final def censor[W] = CensorApply[W]
  final def pass[W] = PassApply[W]


  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class MuteApply[W]:
    def apply[A, U <: @@[W]](body: A !! U): A !! U = polymorphize[W].perform(_.mute(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class ListenApply[W]:
    def apply[A, U <: @@[W]](body: A !! U): (A, W) !! U = polymorphize[W].perform(_.listen(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class CensorApply[W]:
    def apply[A, U <: @@[W]](f: W => W)(body: A !! U): A !! U = polymorphize[W].perform(_.censor(f)(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class PassApply[W]:
    def apply[A, U <: @@[W]](body: (A, W => W) !! U): A !! U = polymorphize[W].perform(_.pass(body))


  /** Predefined handlers for this effect. */
  object handlers:
    def local[W](using W: PlusZero[W]): Handler[Identity, (_, W), @@[W], Any] = polymorphize[W].handler(_.handlers.local)
    def shared[W](using W: PlusZero[W]): Handler[Identity, (_, W), @@[W], IO] = polymorphize[W].handler(_.handlers.shared)

    /** Lile [[local]], but accumulate with given function instead of typeclass. */
    def localFold[W](zero: W, plus: (W, W) => W): Handler[Identity, (_, W), @@[W], Any] = local(using PlusZero.instance(zero, plus))

    /** Lile [[shared]], but accumulate with given function instead of typeclass. */
    def sharedFold[W](zero: W, plus: (W, W) => W): Handler[Identity, (_, W), @@[W], IO] = shared(using PlusZero.instance(zero, plus))


/** Predefined instance of [[PolyWriter]] effect.
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object PolyWriter extends PolyWriterEffect

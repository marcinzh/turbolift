package turbolift.effects
import turbolift.{!!, Effect, Handler}
import turbolift.Extensions._


/** Polymorphic variant of [[StateEffect]].
 *
 * In the monomorphic variant, the `S` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * case object MyState extends StateEffect[Int]  // `S` supplied as `Int`
 *
 * val computation = MyState.get   // `S` inferred from the effect instance
 * }}}
 *
 * In the polymorphic variant, the `S` type parameter is **invariantly** inferred
 * at call sites of effect's operations and handlers:
 *
 * {{{
 * case object MyState extends PolyStateEffect
 *
 * val computation1 = MyState.get[Int]   // `S` inferred as `Int`
 * val computation2 = MyState.put(42)    // `S` inferred as `Int`
 * }}}
 *
 * User must ensure that `S` is inferred as the same type at all call sites,
 * within the scope delimited by this effect's handler.
 * Otherwise, application of the handler will fail to typecheck.
 */
abstract class PolyStateEffect extends Effect.Polymorphic_=(new StateEffect[Any] {}):
  final def get[S]: S !! @@[S] = polymorphize[S].perform(_.get)
  final def gets[S] = new GetsApply[S]
  final def put[S](s: S): Unit !! @@[S] = polymorphize[S].perform(_.put(s))
  final def swap[S](s: S): S !! @@[S] = polymorphize[S].perform(_.swap(s))
  final def modify[S](f: S => S): Unit !! @@[S] = polymorphize[S].perform(_.modify(f))
  final def modifyGet[S](f: S => S): S !! @@[S] = polymorphize[S].perform(_.modifyGet(f))
  final def getModify[S](f: S => S): S !! @@[S] = polymorphize[S].perform(_.getModify(f))
  final def getModifyGet[S](f: S => S): (S, S) !! @@[S] = polymorphize[S].perform(_.getModifyGet(f))
  final def update[S] = new UpdateApply[S]
  final def updateGet[S] = new UpdateGetApply[S]
  final def getUpdate[S] = new GetUpdateApply[S]
  final def getUpdateGet[S] = new GetUpdateGetApply[S]

  final def getsEff[S] = new GetsEffApply[S]
  final def putEff[S] = new PutEffApply[S]
  final def swapEff[S] = new SwapEffApply[S]
  final def modifyEff[S] = new ModifyEffApply[S]
  final def modifyGetEff[S] = new ModifyGetEffApply[S]
  final def getModifyEff[S] = new GetModifyEffApply[S]
  final def getModifyGetEff[S] = new GetModifyGetEffApply[S]
  final def updateEff[S] = new UpdateEffApply[S]
  final def updateGetEff[S] = new UpdateGetEffApply[S]
  final def getUpdateEff[S] = new GetUpdateEffApply[S]
  final def getUpdateGetEff[S] = new GetUpdateGetEffApply[S]

  final def localPut[S] = new LocalPutApply[S]
  final def localPutEff[S] = new LocalPutEffApply[S]
  final def localModify[S] = new LocalModifyApply[S]
  final def localModifyEff[S] = new LocalModifyEffApply[S]


  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class GetsApply[S]:
    def apply[A](f: S => A): A !! @@[S] = polymorphize[S].perform(_.gets(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class UpdateApply[S]:
    def apply[A](f: S => (A, S)): A !! @@[S] = polymorphize[S].perform(_.update(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class UpdateGetApply[S]:
    def apply[A](f: S => (A, S)): (A, S) !! @@[S] = polymorphize[S].perform(_.updateGet(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class GetUpdateApply[S]:
    def apply[A](f: S => (A, S)): (A, S) !! @@[S] = polymorphize[S].perform(_.getUpdate(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class GetUpdateGetApply[S]:
    def apply[A](f: S => (A, S)): (A, S, S) !! @@[S] = polymorphize[S].perform(_.getUpdateGet(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class GetsEffApply[S]:
    def apply[A, U <: @@[S]](f: S => A !! U): A !! U = polymorphize[S].perform(_.getsEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class PutEffApply[S]:
    def apply[U <: @@[S]](s: S !! U): Unit !! U = polymorphize[S].perform(_.putEff(s))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class SwapEffApply[S]:
    def apply[U <: @@[S]](s: S !! U): S !! U = polymorphize[S].perform(_.swapEff(s))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class ModifyEffApply[S]:
    def apply[U <: @@[S]](f: S => S !! U): Unit !! U = polymorphize[S].perform(_.modifyEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class ModifyGetEffApply[S]:
    def apply[U <: @@[S]](f: S => S !! U): S !! U = polymorphize[S].perform(_.modifyGetEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class GetModifyEffApply[S]:
    def apply[U <: @@[S]](f: S => S !! U): S !! U = polymorphize[S].perform(_.getModifyEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class GetModifyGetEffApply[S]:
    def apply[U <: @@[S]](f: S => S !! U): (S, S) !! U = polymorphize[S].perform(_.getModifyGetEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class UpdateEffApply[S]:
    def apply[A, U <: @@[S]](f: S => (A, S) !! U): A !! U = polymorphize[S].perform(_.updateEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class UpdateGetEffApply[S]:
    def apply[A, U <: @@[S]](f: S => (A, S) !! U): (A, S) !! U = polymorphize[S].perform(_.updateGetEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class GetUpdateEffApply[S]:
    def apply[A, U <: @@[S]](f: S => (A, S) !! U): (A, S) !! U = polymorphize[S].perform(_.getUpdateEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class GetUpdateGetEffApply[S]:
    def apply[A, U <: @@[S]](f: S => (A, S) !! U): (A, S, S) !! U = polymorphize[S].perform(_.getUpdateGetEff(f))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class LocalPutApply[S]:
    def apply[A, U <: @@[S]](s: S)(body: A !! U): A !! U = polymorphize[S].perform(_.localPut(s)(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class LocalPutEffApply[S]:
    def apply[A, U <: @@[S]](s: S !! U)(body: A !! U): A !! U = polymorphize[S].perform(_.localPutEff(s)(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class LocalModifyApply[S]:
    def apply[A, U <: @@[S]](f: S => S)(body: A !! U): A !! U = polymorphize[S].perform(_.localModify(f)(body))

  /** Helper class for partial type application. Won't be needed in future Scala (SIP-47). */
  final class LocalModifyEffApply[S]:
    def apply[A, U <: @@[S]](f: S => S !! U)(body: A !! U): A !! U = polymorphize[S].perform(_.localModifyEff(f)(body))


  /** Predefined handlers for this effect. */
  object handlers:
    def local[S](initial: S): Handler[Identity, (_, S), @@[S], Any] = polymorphize[S].handler(_.handlers.local(initial))
    def shared[S](initial: S): Handler[Identity, (_, S), @@[S], IO] = polymorphize[S].handler(_.handlers.shared(initial))


/** Predefined instance of [[PolyState]] effect.
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object PolyState extends PolyStateEffect

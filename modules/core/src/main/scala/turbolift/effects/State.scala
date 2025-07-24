package turbolift.effects
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.io.AtomicVar


/** Signature of [[StateEffect]]. */
trait StateSignature[S] extends Signature:
  def get: S !! ThisEffect
  def gets[A](f: S => A): A !! ThisEffect
  def put(s: S): Unit !! ThisEffect
  def swap(s: S): S !! ThisEffect
  def modify(f: S => S): Unit !! ThisEffect
  def modifyGet(f: S => S): S !! ThisEffect
  def getModify(f: S => S): S !! ThisEffect
  def getModifyGet(f: S => S): (S, S) !! ThisEffect
  def update[A](f: S => (A, S)): A !! ThisEffect
  def updateGet[A](f: S => (A, S)): (A, S) !! ThisEffect
  def getUpdate[A](f: S => (A, S)): (A, S) !! ThisEffect
  def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! ThisEffect

  def getsEff[A, U <: ThisEffect](f: S => A !! U): A !! U
  def putEff[U <: ThisEffect](s: S !! U): Unit !! U
  def swapEff[U <: ThisEffect](s: S !! U): S !! U
  def modifyEff[U <: ThisEffect](f: S => S !! U): Unit !! U
  def modifyGetEff[U <: ThisEffect](f: S => S !! U): S !! U
  def getModifyEff[U <: ThisEffect](f: S => S !! U): S !! U
  def getModifyGetEff[U <: ThisEffect](f: S => S !! U): (S, S) !! U
  def updateEff[A, U <: ThisEffect](f: S => (A, S) !! U): A !! U
  def updateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U
  def getUpdateEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U
  def getUpdateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S, S) !! U

  def localPut[A, U <: ThisEffect](s: S)(body: A !! U): A !! U
  def localPutEff[A, U <: ThisEffect](s: S !! U)(body: A !! U): A !! U
  def localModify[A, U <: ThisEffect](f: S => S)(body: A !! U): A !! U
  def localModifyEff[A, U <: ThisEffect](f: S => S !! U)(body: A !! U): A !! U


/** Base trait for custom instances of State effect.
 *
 * {{{
 * case object MyState extends StateEffect[Int]
 * // optional:
 * type MyState = MyState.type
 * }}}
 *
 * @see [[PolyStateEffect]]
 * @see [[State]]
 */
trait StateEffect[S] extends Effect[StateSignature[S]] with StateSignature[S]:
  enclosing =>
  final override val get: S !! this.type = perform(_.get)
  final override def gets[A](f: S => A): A !! this.type = perform(_.gets(f))
  final override def put(s: S): Unit !! this.type = perform(_.put(s))
  final override def swap(s: S): S !! this.type = perform(_.swap(s))
  final override def modify(f: S => S): Unit !! this.type = perform(_.modify(f))
  final override def modifyGet(f: S => S): S !! this.type = perform(_.modifyGet(f))
  final override def getModify(f: S => S): S !! this.type = perform(_.getModify(f))
  final override def getModifyGet(f: S => S): (S, S) !! this.type = perform(_.getModifyGet(f))
  final override def update[A](f: S => (A, S)): A !! this.type = perform(_.update(f))
  final override def updateGet[A](f: S => (A, S)): (A, S) !! this.type = perform(_.updateGet(f))
  final override def getUpdate[A](f: S => (A, S)): (A, S) !! this.type = perform(_.getUpdate(f))
  final override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! this.type = perform(_.getUpdateGet(f))

  final override def getsEff[A, U <: this.type](f: S => A !! U): A !! U = perform(_.getsEff(f))
  final override def putEff[U <: this.type](s: S !! U): Unit !! U = perform(_.putEff(s))
  final override def swapEff[U <: this.type](s: S !! U): S !! U = perform(_.swapEff(s))
  final override def modifyEff[U <: this.type](f: S => S !! U): Unit !! U = perform(_.modifyEff(f))
  final override def modifyGetEff[U <: this.type](f: S => S !! U): S !! U = perform(_.modifyGetEff(f))
  final override def getModifyEff[U <: this.type](f: S => S !! U): S !! U = perform(_.getModifyEff(f))
  final override def getModifyGetEff[U <: this.type](f: S => S !! U): (S, S) !! U = perform(_.getModifyGetEff(f))
  final override def updateEff[A, U <: this.type](f: S => (A, S) !! U): A !! U = perform(_.updateEff(f))
  final override def updateGetEff[A, U <: this.type](f: S => (A, S) !! U): (A, S) !! U = perform(_.updateGetEff(f))
  final override def getUpdateEff[A, U <: this.type](f: S => (A, S) !! U): (A, S) !! U = perform(_.getUpdateEff(f))
  final override def getUpdateGetEff[A, U <: this.type](f: S => (A, S) !! U): (A, S, S) !! U = perform(_.getUpdateGetEff(f))

  final override def localPut[A, U <: this.type](s: S)(body: A !! U): A !! U = perform(_.localPut(s)(body))
  final override def localPutEff[A, U <: this.type](s: S !! U)(body: A !! U): A !! U = perform(_.localPutEff(s)(body))
  final override def localModify[A, U <: this.type](f: S => S)(body: A !! U): A !! U = perform(_.localModify(f)(body))
  final override def localModifyEff[A, U <: this.type](f: S => S !! U)(body: A !! U): A !! U = perform(_.localModifyEff(f)(body))

  /** Predefined handlers for this effect. */
  object handlers:
    def local(initial: S): Handler[Identity, (_, S), enclosing.type, Any] =
      new impl.Stateful[Identity, (_, S), Any] with impl.Sequential.Restartable with StateSignature[S]:
        override type Local = S
        override def onInitial: S !! Any = !!.pure(initial)
        override def onReturn(a: Unknown, s: S): (Unknown, S) !! Any = !!.pure((a, s))
        override def onRestart(a_s: (Unknown, S)): Unknown !! enclosing.type = enclosing.put(a_s._2) &&! !!.pure(a_s._1)
        override def onOnce(aa: (Unknown, S)): Option[Unknown] = Some(aa._1)

        override val get: S !! ThisEffect = Local.get
        override def gets[A](f: S => A): A !! ThisEffect = Local.get.map(f)
        override def put(s2: S): Unit !! ThisEffect = Local.put(s2)
        override def swap(s2: S): S !! ThisEffect = Local.swap(s2)
        override def modify(f: S => S): Unit !! ThisEffect = Local.modify(f)
        override def modifyGet(f: S => S): S !! ThisEffect = Local.modifyGet(f)
        override def getModify(f: S => S): S !! ThisEffect = Local.getModify(f)
        override def getModifyGet(f: S => S): (S, S) !! ThisEffect = Local.getModifyGet(f)
        override def update[A](f: S => (A, S)): A !! ThisEffect = Local.update(f)
        override def updateGet[A](f: S => (A, S)): (A, S) !! ThisEffect = Local.updateGet(f)
        override def getUpdate[A](f: S => (A, S)): (A, S) !! ThisEffect = Local.getUpdate(f)
        override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! ThisEffect = Local.getUpdateGet(f)
        override def getsEff[A, U <: ThisEffect](f: S => A !! U): A !! U = Local.getsEff(f)
        override def putEff[U <: ThisEffect](s: S !! U): Unit !! U = Local.putEff(s)
        override def swapEff[U <: ThisEffect](s: S !! U): S !! U = Local.swapEff(s)
        override def modifyEff[U <: ThisEffect](f: S => S !! U): Unit !! U = Local.modifyEff(f)
        override def modifyGetEff[U <: ThisEffect](f: S => S !! U): S !! U = Local.modifyGetEff(f)
        override def getModifyEff[U <: ThisEffect](f: S => S !! U): S !! U = Local.getModifyEff(f)
        override def getModifyGetEff[U <: ThisEffect](f: S => S !! U): (S, S) !! U = Local.getModifyGetEff(f)
        override def updateEff[A, U <: ThisEffect](f: S => (A, S) !! U): A !! U = Local.updateEff(f)
        override def updateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U = Local.updateGetEff(f)
        override def getUpdateEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U = Local.getUpdateEff(f)
        override def getUpdateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S, S) !! U = Local.getUpdateGetEff(f)
        override def localPut[A, U <: ThisEffect](s: S)(body: A !! U): A !! U = Control.delimitPut(body, s).map(_._1)
        override def localPutEff[A, U <: ThisEffect](s: S !! U)(body: A !! U): A !! U = s.flatMap(Control.delimitPut(body, _)).map(_._1)
        override def localModify[A, U <: ThisEffect](f: S => S)(body: A !! U): A !! U = Control.delimitModify(body, f).map(_._1)
        override def localModifyEff[A, U <: ThisEffect](f: S => S !! U)(body: A !! U): A !! U = Local.getsEff(f).flatMap(Control.delimitPut(body, _)).map(_._1)
      .toHandler

    def shared(initial: S): Handler[Identity, (_, S), enclosing.type, IO] =
      AtomicVar(initial).flatMapHandler: avar =>
        new impl.Proxy[IO] with StateSignature[S]:
          override def get: S !! ThisEffect = avar.get
          override def gets[A](f: S => A): A !! ThisEffect = avar.gets(f)
          override def put(s: S): Unit !! ThisEffect = avar.put(s)
          override def swap(s: S): S !! ThisEffect = avar.swap(s)
          override def modify(f: S => S): Unit !! ThisEffect = avar.modify(f)
          override def modifyGet(f: S => S): S !! ThisEffect = avar.modifyGet(f)
          override def getModify(f: S => S): S !! ThisEffect = avar.getModify(f)
          override def getModifyGet(f: S => S): (S, S) !! ThisEffect = avar.getModifyGet(f)
          override def update[A](f: S => (A, S)): A !! ThisEffect = avar.update(f)
          override def updateGet[A](f: S => (A, S)): (A, S) !! ThisEffect = avar.updateGet(f)
          override def getUpdate[A](f: S => (A, S)): (A, S) !! ThisEffect = avar.getUpdate(f)
          override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! ThisEffect = avar.getUpdateGet(f)
          override def getsEff[A, U <: ThisEffect](f: S => A !! U): A !! U = avar.getsEff(f)
          override def putEff[U <: ThisEffect](s: S !! U): Unit !! U = avar.putEff(s)
          override def swapEff[U <: ThisEffect](s: S !! U): S !! U = avar.swapEff(s)
          override def modifyEff[U <: ThisEffect](f: S => S !! U): Unit !! U = avar.modifyEff(f)
          override def modifyGetEff[U <: ThisEffect](f: S => S !! U): S !! U = avar.modifyGetEff(f)
          override def getModifyEff[U <: ThisEffect](f: S => S !! U): S !! U = avar.getModifyEff(f)
          override def getModifyGetEff[U <: ThisEffect](f: S => S !! U): (S, S) !! U = avar.getModifyGetEff(f)
          override def updateEff[A, U <: ThisEffect](f: S => (A, S) !! U): A !! U = avar.updateEff(f)
          override def updateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U = avar.updateGetEff(f)
          override def getUpdateEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S) !! U = avar.getUpdateEff(f)
          override def getUpdateGetEff[A, U <: ThisEffect](f: S => (A, S) !! U): (A, S, S) !! U = avar.getUpdateGetEff(f)
          override def localPut[A, U <: ThisEffect](s: S)(body: A !! U): A !! U = avar.swap(s).flatMap(locally(body))
          override def localPutEff[A, U <: ThisEffect](s: S !! U)(body: A !! U): A !! U = avar.swapEff(s).flatMap(locally(body))
          override def localModify[A, U <: ThisEffect](f: S => S)(body: A !! U): A !! U = avar.getModify(f).flatMap(locally(body))
          override def localModifyEff[A, U <: ThisEffect](f: S => S !! U)(body: A !! U): A !! U = avar.getModifyEff(f).flatMap(locally(body))
          private def locally[A, U <: ThisEffect](body: A !! U)(s: S): A !! U = body &&<! avar.put(s)
        .toHandler
        .mapEffK([A] => (a: A) => avar.get.map((a, _)))


object StateEffect:
  extension [S](thiz: StateEffect[S])
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler(initial: S): Handler[Identity, (_, S), thiz.type, Any] = thiz.handlers.local(initial)


/** Polymorphic variant of [[StateEffect]].
 *
 * In the monomorphic variant, the `S` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * // The `S` is explicitly set as `Int`:
 * case object MyState extends StateEffect[Int]
 *
 * // The `S` is inferred from the effect instance:
 * val computation = MyState.get
 * }}}
 *
 * In the polymorphic variant, the `S` type parameter is **invariantly** inferred
 * at call sites of effect's operations and handlers:
 *
 * {{{
 * case object MyState extends PolyStateEffect
 *
 * val computation1 = MyState.put(42)      // `S` inferred as `Int`
 * val computation2 = MyState.get[Int]     // `S` inferred as `Int`
 * val computation3 = MyState.get[String]  // `S` inferred as `String`
 *
 * // Inferred types of the above computations:
 * val _: Unit !! MyState.@@[Int]    = computation1
 * val _: Unit !! MyState.@@[Int]    = computation2
 * val _: Unit !! MyState.@@[String] = computation3
 *
 * // Type error!
 * // Since `S` is invariant, compiler can't unify `S` as `Int` with `S` as `String`
 * val computation4 = computation2 &&! computation3
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


object PolyStateEffect:
  extension (thiz: PolyStateEffect)
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler[S](initial: S): Handler[Identity, (_, S), thiz.@@[S], Any] = thiz.handlers.local(initial)


/** Predefined instance of [[PolyStateEffect]].
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object State extends PolyStateEffect
type State[S] = State.@@[S]

package turbolift.effects
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.data.Resource


/** Signature of [[ReaderEffect]]. */
trait ReaderSignature[R] extends Signature:
  def ask: R !! ThisEffect
  def asks[A](f: R => A): A !! ThisEffect
  def asksEff[A, U <: ThisEffect](f: R => A !! U): A !! U
  def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !! U
  def localPutEff[A, U <: ThisEffect](r: R !! U)(body: A !! U): A !! U
  def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !! U
  def localModifyEff[A, U <: ThisEffect](f: R => R !! U)(body: A !! U): A !! U


/** Base trait for custom instances of Reader effect.
 *
 * {{{
 * case object MyReader extends ReaderEffect[Int]
 * // optional:
 * type MyReader = MyReader.type
 * }}}
 *
 * @see [[PolyReaderEffect]]
 * @see [[Reader]]
 */
trait ReaderEffect[R] extends Effect[ReaderSignature[R]] with ReaderSignature[R]:
  enclosing =>
  final override val ask: R !! this.type = perform(_.ask)
  final override def asks[A](f: R => A): A !! this.type = perform(_.asks(f))
  final override def asksEff[A, U <: this.type](f: R => A !! U): A !! U = perform(_.asksEff(f))
  final override def localPut[A, U <: this.type](r: R)(body: A !! U): A !! U = perform(_.localPut(r)(body))
  final override def localPutEff[A, U <: this.type](r: R !! U)(body: A !! U): A !! U = perform(_.localPutEff(r)(body))
  final override def localModify[A, U <: this.type](f: R => R)(body: A !! U): A !! U = perform(_.localModify(f)(body))
  final override def localModifyEff[A, U <: this.type](f: R => R !! U)(body: A !! U): A !! U = perform(_.localModifyEff(f)(body))


  /** Predefined handlers for this effect. */
  object handlers:
    def default(initial: R): Handler[Identity, Identity, enclosing.type, Any] =
      new impl.Stateful[Identity, Identity, Any] with impl.Parallel.Trivial with ReaderSignature[R]:
        override type Local = R
        override def onInitial: R !! Any = !!.pure(initial)
        override def onReturn(a: Unknown, r: R): Unknown !! Any = !!.pure(a)

        override val ask: R !! ThisEffect = Local.get
        override def asksEff[A, U <: ThisEffect](f: R => A !! U): A !! U = Local.getsEff(f)
        override def asks[A](f: R => A): A !! ThisEffect = Local.gets(f)
        override def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !! U = Control.delimitPut(body, r)
        override def localPutEff[A, U <: ThisEffect](r: R !! U)(body: A !! U): A !! U = r.flatMap(Control.delimitPut(body, _))
        override def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !! U = Control.delimitModify(body, f)
        override def localModifyEff[A, U <: ThisEffect](f: R => R !! U)(body: A !! U): A !! U = Local.getsEff(f).flatMap(Control.delimitPut(body, _))
      .toHandler


    /** Auxiliary handler for [[fromLazyResource]]. */
    def lazily[V](acquire: R !! V): Handler[Identity, Identity, enclosing.type, V] =
      new impl.Stateful[Identity, Identity, V] with impl.Parallel.Trivial with ReaderSignature[R]:
        override type Local = Option[R]
        override def onInitial = !!.none
        override def onReturn(x: Unknown, r: Local) = x.pure_!!

        override val ask: R !! ThisEffect = force
        override def asksEff[A, U <: ThisEffect](f: R => A !! U): A !! U = ask.flatMap(f)
        override def asks[A](f: R => A): A !! ThisEffect = ask.map(f)
        override def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !! U = localModify(_ => r)(body)
        override def localPutEff[A, U <: ThisEffect](r: R !! U)(body: A !! U): A !! U = localModifyEff(_ => r)(body)
        override def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !! U = localModifyEff(r => f(r).pure_!!)(body)
        override def localModifyEff[A, U <: ThisEffect](f: R => R !! U)(body: A !! U): A !! U = force.flatMap(f).flatMap(r => Control.delimitPut(body, Some(r)))

        val force: R !! ThisEffect =
          Local.getsEff:
            case Some(r) => r.pure_!!
            case None => acquire.tapEff(r => Local.put(Some(r)))

      .toHandler


    def fromResource[V](resource: Resource[R, V]): Handler.IdId[enclosing.type, V] =
      Handler.fromFunction:
        [A, U] => (comp: A !! (enclosing.type & U)) =>
          resource.use: r =>
            comp.handleWith(default(r))


    def fromLazyResource[V <: IO](resource: Resource[R, V]): Handler.IdId[enclosing.type, V] =
      Handler.fromFunction:
        [A, U] => (comp: A !! (enclosing.type & U)) =>
          resource.useLazily: getter =>
            comp.handleWith(lazily(getter))


object ReaderEffect:
  extension [R](thiz: ReaderEffect[R])
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler(initial: R): Handler[Identity, Identity, thiz.type, Any] = thiz.handlers.default(initial)


/** Polymorphic variant of [[ReaderEffect]].
 *
 * In the monomorphic variant, the `R` type parameter is supplied during creation of an instance of the effect:
 * {{{
 * // The `R` is explicitly set as `String`:
 * case object MyReader extends ReaderEffect[String]
 *
 * // The `R` is inferred from the effect instance:
 * val computation = MyReader.ask
 * }}}
 *
 * In the polymorphic variant, the `R` type parameter is **covariantly** inferred
 * at call sites of effect's operations and handlers
 * In practice, the type can "grow as you go":
 *
 * {{{
 * case object MyReader extends PolyReaderEffect
 *
 * val computation1 = MyReader.ask[Int]              // `R` inferred as `Int`
 * val computation2 = MyReader.ask[String]           // `R` inferred as `String`
 * val computation3 = computation1 **! computation2  // `R` inferred as `Int & String`
 *
 * // Inferred types of the above computations:
 * val _: Int           !! MyReader.@@[Int]          = computation1
 * val _: String        !! MyReader.@@[String]       = computation2
 * val _: (Int, String) !! MyReader.@@[Int & String] = computation3
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
    def default[R](initial: R): Handler.IdId[@@[R], Any] = polymorphize[R].handler(_.handlers.default(initial))
    def lazily[R, V](acquire: R !! V): Handler.IdId[@@[R], V] = polymorphize[R].handler(_.handlers.lazily(acquire))
    def fromResource[R, V](resource: Resource[R, V]): Handler.IdId[@@[R], V] = polymorphize[R].handler(_.handlers.fromResource(resource))
    def fromLazyResource[R, V <: IO](resource: Resource[R, V]): Handler.IdId[@@[R], V] = polymorphize[R].handler(_.handlers.fromLazyResource(resource))


object PolyReaderEffect:
  extension (thiz: PolyReaderEffect)
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler[R](initial: R): Handler[Identity, Identity, thiz.@@[R], Any] = thiz.handlers.default(initial)


/** Predefined instance of [[PolyReaderEffect]].
 *
 * Note that using predefined effect instances like this, is anti-modular.
 * However, they can be convenient in exploratory code.
 */
case object Reader extends PolyReaderEffect
type Reader[R] = Reader.@@[R]

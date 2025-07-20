package turbolift.effects
import scala.util.{Try, Success, Failure}
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._


/** Signature of [[MaybeEffect]]. */
trait MaybeSignature extends Signature:
  def empty: Nothing !! ThisEffect
  def catchToOption[A, U <: ThisEffect](body: A !! U): Option[A] !! U


/** Base trait for custom instances of Maybe effect.
 *
 * {{{
 * case object MyMaybe extends MaybeEffect
 * // optional:
 * type MyMaybe = MyMaybe.type
 * }}}
 *
 * @see [[Maybe]]
 * @see [[Broken]]
 */
trait MaybeEffect extends Effect[MaybeSignature] with MaybeSignature:
  enclosing =>
  final override val empty: Nothing !! this.type = perform(_.empty)
  final override def catchToOption[A, U <: this.type](body: A !! U): Option[A] !! U = perform(_.catchToOption(body))

  final def fromOption[A](x: Option[A]): A !! this.type = x.fold(empty)(!!.pure)
  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x.fold(_ => empty, !!.pure)
  final def fromTry[A](x: Try[A]): A !! this.type = x.fold(_ => empty, !!.pure)

  /** Predefined handlers for this effect. */
  object handlers:
    def toOption: Handler[Identity, Option, enclosing.type, Any] =
      new impl.Stateless[Identity, Option, Any] with impl.Parallel with MaybeSignature:
        override def onReturn(a: Unknown): Option[Unknown] !! Any = !!.pure(Some(a))
        override def onRestart(aa: Option[Unknown]): Unknown !! enclosing.type = aa.fold(enclosing.empty)(!!.pure)
        override def onUnknown(aa: Option[Unknown]): Option[Unknown] = aa
        override def onZip[A, B, C](aa: Option[A], bb: Option[B], k: (A, B) => C): Option[C] = aa.zip(bb).map(k.tupled)

        override def empty: Nothing !! ThisEffect = Control.abort(None)
        override def catchToOption[A, U <: ThisEffect](body: A !! U): Option[A] !! U = Control.delimit(body)
      .toHandler

    def default: Handler[Identity, Option, enclosing.type, Any] = toOption
    def orElse(e: => Nothing): Handler[Identity, Identity, enclosing.type, Any] = default.mapK([X] => (xx: Option[X]) => xx.fold(e)(x => x))
    def orCancel: Handler[Identity, Identity, enclosing.type, IO] = default.mapEffK([X] => (xx: Option[X]) => xx.fold(IO.cancel)(!!.pure))
    def orElseCancel: Handler[Identity, Identity, enclosing.type, IO] = default.mapEffK([X] => (xx: Option[X]) => xx.fold(IO.cancel)(!!.pure))


object MaybeEffect:
  extension (thiz: MaybeEffect)
    /** Alias of the default handler for this effect.
     *
     * Defined as an extension, to allow custom redefinitions without restrictions imposed by overriding
     */
    def handler: Handler[Identity, Option, thiz.type, Any] = thiz.handlers.default


/** Predefined instance of [[MaybeEffect]]. */
case object Maybe extends MaybeEffect
type Maybe = Maybe.type


/** Predefined instance of [[MaybeEffect]], used by `io.EffectfulVar`. */
case object Broken extends MaybeEffect:
  export handlers.{toOption, orElse => getOrElse, orCancel => getOrCancel}

type Broken = Broken.type

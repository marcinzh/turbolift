package turbolift.effects
import scala.util.{Try, Success, Failure}
import turbolift.{!!, Effect, Signature}
import turbolift.handlers.maybeHandler


trait MaybeSignature extends Signature:
  def empty: Nothing !! ThisEffect
  def catchToOption[A, U <: ThisEffect](body: A !! U): Option[A] !! U


trait MaybeEffect extends Effect[MaybeSignature] with MaybeSignature:
  final override val empty: Nothing !! this.type = perform(_.empty)
  final override def catchToOption[A, U <: this.type](body: A !! U): Option[A] !! U = perform(_.catchToOption(body))

  final def fromOption[A](x: Option[A]): A !! this.type = x.fold(empty)(!!.pure)
  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x.fold(_ => empty, !!.pure)
  final def fromTry[A](x: Try[A]): A !! this.type = x.fold(_ => empty, !!.pure)

  /** Predefined handlers for this effect. */
  object handlers:
    def default: ThisHandler[Identity, Option, Any] = toOption
    def toOption: ThisHandler[Identity, Option, Any] = MaybeEffect.this.maybeHandler
    def orElse(e: => Nothing): ThisHandler[Identity, Identity, Any] = default.mapK([X] => (xx: Option[X]) => xx.fold(e)(x => x))
    def orCancel: ThisHandler[Identity, Identity, IO] = default.mapEffK([X] => (xx: Option[X]) => xx.fold(IO.cancel)(!!.pure))
    def orElseCancel: ThisHandler[Identity, Identity, IO] = default.mapEffK([X] => (xx: Option[X]) => xx.fold(IO.cancel)(!!.pure))


trait Maybe extends MaybeEffect:
  export handlers.{default => handler}

/** Predefined instance of this effect. */
case object Maybe extends Maybe


/** Predefined instance of this effect, used by `io.EffectfulVar`. */
case object Broken extends Maybe:
  export handlers.{toOption, orElse => getOrElse, orCancel => getOrCancel}

type Broken = Broken.type

package turbolift.effects
import scala.util.Try
import turbolift.{!!, Effect, Signature}
import turbolift.effects.default_handlers.{choiceHandler_first, choiceHandler_all} 


trait ChoiceSig extends Signature:
  def fail: Nothing !@! ThisEffect
  def choose[A](as: Iterable[A]): A !@! ThisEffect


sealed trait ChoiceEffect extends Effect[ChoiceSig] with ChoiceSig:
  final override val fail: Nothing !! this.type = perform(_.fail)
  final override def choose[A](as: Iterable[A]): A !! this.type = perform(_.choose(as))

  final def plus[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = choose(Vector(lhs, !!.defer(rhs))).flatten

  final def apply[A](as: A*): A !! this.type = choose(as.toVector)

  final def fromOption[A](x: Option[A]): A !! this.type = x.fold(fail)(pure)
  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x.fold(_ => fail, pure)
  final def fromTry[A](x: Try[A]): A !! this.type = x.fold(_ => fail, pure)

  /** Predefined handlers for this effect. */
  object handlers:
    def first: ThisHandler.Free[Option] = ChoiceEffect.this.choiceHandler_first
    def all: ThisHandler.Free[Vector] = ChoiceEffect.this.choiceHandler_all


trait Choice extends ChoiceEffect:
  /** Default handler for this effect. */
  def handler: ThisHandler.Free[Vector] = handlers.all

case object Fail extends ChoiceEffect:
  /** Default handler for this effect. */
  def handler: ThisHandler.Free[Option] = handlers.first

type Fail = Fail.type

case object Each extends Choice:
  def void: ThisHandler.FreeConst[Unit] = handlers.all.void

type Each = Each.type

package turbolift.std_effects
import scala.util.Try
import turbolift.{!!, Effect, Signature}
import turbolift.std_effects.default_handlers.{ChoiceHandler_One, ChoiceHandler_Many} 


trait ChoiceSig extends Signature:
  def fail: Nothing !@! ThisEffect
  def orElse[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !@! U
  def choose[A](as: Iterable[A]): A !@! ThisEffect


sealed trait ChoiceEffect extends Effect[ChoiceSig] with ChoiceSig:
  final override val fail: Nothing !! this.type = perform(_.fail)
  final override def orElse[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = perform(_.orElse(lhs, rhs))
  final override def choose[A](as: Iterable[A]): A !! this.type = perform(_.choose(as))

  final def empty = fail
  final def plus[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = orElse(lhs, rhs)
  final def apply[A](as: A*): A !! this.type = choose(as.toVector)

  final def fromOption[A](x: Option[A]): A !! this.type = x.fold(empty)(pure)
  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x.fold(_ => empty, pure)
  final def fromTry[A](x: Try[A]): A !! this.type = x.fold(_ => empty, pure)

  object handlers:
    val one: ThisHandler.Free[Option] = ChoiceHandler_One(ChoiceEffect.this)
    val many: ThisHandler.Free[Vector] = ChoiceHandler_Many(ChoiceEffect.this)


trait Choice extends ChoiceEffect:
  def handler: ThisHandler.Free[Vector] = handlers.many

case object Fail extends ChoiceEffect:
  def handler: ThisHandler.Free[Option] = handlers.one

type Fail = Fail.type

case object Each extends Choice:
  def void: ThisHandler.FreeConst[Unit] = handlers.many.void

type Each = Each.type

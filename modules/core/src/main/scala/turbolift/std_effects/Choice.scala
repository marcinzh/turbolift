package turbolift.std_effects
import scala.util.{Try, Success}
import turbolift.abstraction.{!!, Effect, Signature}


trait ChoiceSig extends Signature:
  def empty: Nothing !@! ThisEffect
  def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !@! U
  def each[A](as: Iterable[A]): A !@! ThisEffect


trait ChoiceExt extends Effect[ChoiceSig] with ChoiceSig:
  final override val empty: Nothing !! this.type = impure(_.empty)
  final override def plus[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = impure(_.plus(lhs, rhs))
  final override def each[A](as: Iterable[A]): A !! this.type = impure(_.each(as))

  final def apply[A](as: A*): A !! this.type = each(as.toVector)
  final def fail = empty

  final def fromOption[A](x: Option[A]): A !! this.type = x.fold(empty)(pure)
  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x.fold(_ => empty, pure)
  final def fromTry[A](x: Try[A]): A !! this.type = x.fold(_ => empty, pure)

  object handlers:
    val one: ThisIHandler[Option] = ChoiceHandler_One(ChoiceExt.this)
    val many: ThisIHandler[Vector] = ChoiceHandler_Many(ChoiceExt.this)


trait Choice extends ChoiceExt:
  def handler: ThisIHandler[Vector] = handlers.many

trait Fail extends ChoiceExt:
  def handler: ThisIHandler[Option] = handlers.one

case object Each extends Choice:
  def void: ThisIHandler[[X] =>> Unit] = handlers.many.void

type Each = Each.type

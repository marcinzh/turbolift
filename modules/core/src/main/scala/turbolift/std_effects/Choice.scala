package turbolift.std_effects
import scala.util.Try
import turbolift.{!!, Effect, Signature}
import turbolift.std_effects.default_handlers.{ChoiceHandler_One, ChoiceHandler_Many} 


trait ChoiceSig extends FailSig:
  def choose[A](as: Iterable[A]): A !@! ThisEffect


trait Choice extends Effect[ChoiceSig] with ChoiceSig:
  final override val fail: Nothing !! this.type = perform(_.fail)
  final override def orElse[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = perform(_.orElse(lhs, rhs))
  final override def choose[A](as: Iterable[A]): A !! this.type = perform(_.choose(as))

  final def empty = fail
  final def plus[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = orElse(lhs, rhs)
  final def apply[A](as: A*): A !! this.type = choose(as.toVector)

  final def fromOption[A](x: Option[A]): A !! this.type = x.fold(empty)(pure)
  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x.fold(_ => empty, pure)
  final def fromTry[A](x: Try[A]): A !! this.type = x.fold(_ => empty, pure)

  def handler: ThisHandler.Free[Vector] = handlers.many

  object handlers:
    val one: ThisHandler.Free[Option] = ChoiceHandler_One(Choice.this)
    val many: ThisHandler.Free[Vector] = ChoiceHandler_Many(Choice.this)

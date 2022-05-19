package turbolift.std_effects
import scala.util.Try
import turbolift.{!!, Effect, Signature}
import turbolift.std_effects.default_handlers.FailHandler


trait FailSig extends Signature:
  def fail: Nothing !@! ThisEffect
  def orElse[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !@! U


trait FailEffect extends Effect[FailSig] with FailSig:
  final override val fail: Nothing !! this.type = perform(_.fail)
  final override def orElse[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = perform(_.orElse(lhs, rhs))

  final def empty = fail
  final def plus[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = orElse(lhs, rhs)

  final def fromOption[A](x: Option[A]): A !! this.type = x.fold(fail)(pure)
  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x.fold(_ => fail, pure)
  final def fromTry[A](x: Try[A]): A !! this.type = x.fold(_ => fail, pure)

  def handler: ThisHandler.Free[Option] = FailHandler(this)


case object Fail extends FailEffect
type Fail = Fail.type

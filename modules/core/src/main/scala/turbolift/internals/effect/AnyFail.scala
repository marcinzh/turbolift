package turbolift.internals.effect
import scala.util.Try
import turbolift.!!
import turbolift.std_effects.FailSig


case object AnyFail extends ProtoEffect[FailSig] with FailSig:
  override type ThisEffect >: FailSig

  final override val fail: Nothing !! ThisEffect = perform(_.fail)
  final override def orElse[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = perform(_.orElse(lhs, rhs))

  final def empty = fail
  final def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = orElse(lhs, rhs)

  final def fromOption[A](x: Option[A]): A !! ThisEffect = x.fold(fail)(pure)
  final def fromEither[E, A](x: Either[E, A]): A !! ThisEffect = x.fold(_ => fail, pure)
  final def fromTry[A](x: Try[A]): A !! ThisEffect = x.fold(_ => fail, pure)

type AnyFail = AnyFail.ThisEffect

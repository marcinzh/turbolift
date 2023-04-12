package turbolift.internals.effect
import scala.util.Try
import turbolift.!!
import turbolift.effects.ChoiceSignature


private[turbolift] case object AnyChoice extends CanPerform[ChoiceSignature] with ChoiceSignature:
  override type ThisEffect >: ChoiceSignature

  final override val fail: Nothing !! ThisEffect = perform(_.fail)
  final override def choose[A](as: Iterable[A]): A !! ThisEffect = perform(_.choose(as))

  final def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = choose(Vector(lhs, !!.defer(rhs))).flatten

  final def apply[A](as: A*): A !! ThisEffect = choose(as.toVector)

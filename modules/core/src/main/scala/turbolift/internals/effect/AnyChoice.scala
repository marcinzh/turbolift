package turbolift.internals.effect
import turbolift.!!
import turbolift.effects.ChoiceSignature
import turbolift.internals.primitives.{ComputationCases => CC}



private[turbolift] case object AnyChoice extends CanPerform[ChoiceSignature] with ChoiceSignature:
  override type ThisEffect >: ChoiceSignature

  override val fail: Nothing !! ThisEffect = perform(_.fail)
  override def choose[A](as: Iterable[A]): A !! ThisEffect = perform(_.choose(as))

  def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = choose(Vector(lhs, !!.defer(rhs))).flatten

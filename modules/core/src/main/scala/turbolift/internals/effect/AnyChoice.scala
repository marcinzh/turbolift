package turbolift.internals.effect
import turbolift.!!
import turbolift.std_effects.{Choice, ChoiceSig}


case object AnyChoice extends ProtoEffect[ChoiceSig] with HasEffectId.Nul:
  override type ThisEffect = Choice

  final val empty: Nothing !! ThisEffect = operate(_.empty)
  final def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = operate(_.plus(lhs, rhs))

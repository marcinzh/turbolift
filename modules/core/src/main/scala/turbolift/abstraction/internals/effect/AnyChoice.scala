package turbolift.abstraction.internals.effect
import turbolift.abstraction.!!
import turbolift.std_effects.{ChoiceExt, ChoiceSig}


case object AnyChoice extends ProtoEffect[ChoiceSig] with HasEffectId.Nul:
  override type ThisEffect = ChoiceExt

  final val empty: Nothing !! ThisEffect = impure(_.empty)
  final def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = impure(_.plus(lhs, rhs))

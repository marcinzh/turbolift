package turbolift.abstraction.internals.effect
import turbolift.abstraction.!!
import turbolift.std_effects.{ChoiceExt, ChoiceSig}


case object AnyChoice extends ProtoEffect[ChoiceSig] with HasEffectId.Nul:
  override type ThisEffect = ChoiceExt

  final val empty: Nothing !! ThisEffect = impureFO(_.empty)
  final def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !! U = impureHO[U](_.plus(lhs, rhs))

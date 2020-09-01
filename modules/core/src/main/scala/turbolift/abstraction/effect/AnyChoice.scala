package turbolift.abstraction.effect
import turbolift.abstraction.!!
import turbolift.std_effects.{Choice, ChoiceSig}


case object AnyChoice extends EffectEncoding[ChoiceSig] with HasEffectId.Nul {
  override type ThisEffect = Choice

  final val empty: Nothing !! ThisEffect = encodeFO(_.empty)
  final def plus[A, U](lhs: A !! U, rhs: => A !! U): A !! U with ThisEffect = encodeHO[U](_.plus(lhs, rhs))
  final def each[A](as: Iterable[A]): A !! ThisEffect = encodeFO(_.each(as))

  final def from[A](x: Option[A]): A !! ThisEffect = x match {
    case Some(a) => pure(a)
    case _ => empty
  }
}

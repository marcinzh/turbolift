package turbolift.abstraction.internals.effect
import turbolift.abstraction.!!
import turbolift.std_effects.{Choice, ChoiceSig}


case object AnyChoice extends Embedding[ChoiceSig] with HasEffectId.Nul {
  override type ThisEffect = Choice

  final val empty: Nothing !! ThisEffect = embedFO(_.empty)
  final def plus[A, U](lhs: A !! U, rhs: => A !! U): A !! U with ThisEffect = embedHO[U](_.plus(lhs, rhs))
  final def each[A](as: Iterable[A]): A !! ThisEffect = embedFO(_.each(as))

  final def from[A](x: Option[A]): A !! ThisEffect = x match {
    case Some(a) => pure(a)
    case _ => empty
  }
}

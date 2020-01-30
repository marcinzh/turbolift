package turbolift.abstraction.effect
import turbolift.abstraction.!!


trait AlternativeSig[P[_]] extends Signature[P] {
  def empty[A]: P[A]
  def plus[A](lhs: P[A], rhs: => P[A]): P[A]
}

sealed trait AltFx

trait AlternativeEffectEncoding[Z[P[_]] <: AlternativeSig[P]] extends AltFx with EffectEncoding[Z] {
  final val empty: Nothing !! ThisEffect = encodeFO(_.empty)
  final def plus[A, U](lhs: A !! U, rhs: => A !! U): A !! U with ThisEffect = encodeHO[U](run => _.plus(run(lhs), run(rhs)))

  final def from[A](x: Option[A]): A !! ThisEffect = x match {
    case Some(a) => pure(a)
    case _ => empty
  }
}

case object AlternativeEffect extends AlternativeEffectEncoding[AlternativeSig] with HasEffectId.Nul {
  override type ThisEffect = AltFx
}

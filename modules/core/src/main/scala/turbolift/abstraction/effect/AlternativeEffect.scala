package turbolift.abstraction.effect
import turbolift.abstraction.!!


trait AlternativeSig[U] extends Signature[U] {
  def empty[A]: A !! U
  def plus[A](lhs: A !! U, rhs: => A !! U): A !! U
}

sealed trait AltFx

trait AlternativeEffectEncoding[Z[U] <: AlternativeSig[U]] extends AltFx with EffectEncoding[Z] {
  final val empty: Nothing !! ThisEffect = encodeFO(_.empty)
  final def plus[A, U](lhs: A !! U, rhs: => A !! U): A !! U with ThisEffect = encodeHO[U](_.plus(lhs, rhs))

  final def from[A](x: Option[A]): A !! ThisEffect = x match {
    case Some(a) => pure(a)
    case _ => empty
  }
}

case object AlternativeEffect extends AlternativeEffectEncoding[AlternativeSig] with HasEffectId.Nul {
  override type ThisEffect = AltFx
}

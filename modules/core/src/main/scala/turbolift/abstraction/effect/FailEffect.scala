package turbolift.abstraction.effect
import turbolift.abstraction.!!


trait FailSig[P[_]] extends Signature[P] {
  def fail[A]: P[A]
  def orElsePar[A](lhs: P[A], rhs: P[A]): P[A]
  def orElseSeq[A](lhs: P[A], rhs: => P[A]): P[A]
}

sealed trait FailEffect

trait FailEffectEncoding[Z[P[_]] <: FailSig[P]] extends FailEffect with EffectEncoding[Z] {
  final val fail: Nothing !! ThisEffect = encodeFO(_.fail)
  final def orElsePar[A, U](lhs: A !! U, rhs: A !! U): A !! U with ThisEffect = encodeHO[U](run => _.orElsePar(run(lhs), run(rhs)))
  final def orElseSeq[A, U](lhs: A !! U, rhs: => A !! U): A !! U with ThisEffect = encodeHO[U](run => _.orElseSeq(run(lhs), run(rhs)))
}

case object FailEffect extends FailEffectEncoding[FailSig] {
  override type ThisEffect = FailEffect
  override def effectId: AnyRef = null
}

package turbolift.abstraction.effect
import mwords.~>
import turbolift.abstraction.!!
import turbolift.abstraction.ComputationCases.{DispatchFO, DispatchHO}


trait EffectEncoding[Z[P[_]] <: Signature[P]] {
  type ThisEffect
  def effectId: AnyRef

  type Phantom[A] // Stays undefined. Solves the "Can't existentially abstract over parameterized type" problem

  final def encodeFO[A](f: Z[Phantom] => Phantom[A]): A !! ThisEffect = new DispatchFO(effectId, f)

  final def encodeHO[U] = new EncodeHO[U]
  class EncodeHO[U] {
    type Run = (? !! U with ThisEffect) ~> Phantom 
    def apply[A](ff: Run => Z[Phantom] => Phantom[A]): A !! U with ThisEffect = new DispatchHO(effectId, ff)
  }

  final def pure[A](a: A): A !! ThisEffect = !!.pure(a)
}

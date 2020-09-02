package turbolift.abstraction.internals.effect
import cats.~>
import turbolift.abstraction.!!
import turbolift.abstraction.ComputationCases.Dispatch


trait Embedding[Z[U] <: Signature[U]] extends HasEffectId.Unsealed {
  type ThisEffect

  final def embedFO[A](f: Z[ThisEffect] => A !! ThisEffect): A !! ThisEffect = new Dispatch(effectId, f)

  final def embedHO[U] = new EncodeHO[U]
  class EncodeHO[U] {
    def apply[A](f: Z[U with ThisEffect] => A !! U with ThisEffect): A !! U with ThisEffect = new Dispatch(effectId, f)
  }

  final def pure[A](a: A): A !! ThisEffect = !!.pure(a)
}

package turbolift.abstraction.internals.effect
import cats.~>
import turbolift.abstraction.!!
import turbolift.abstraction.ComputationCases.Impure


trait ProtoEffect[Z[_]] extends HasEffectId.Unsealed:
  type ThisEffect

  final def impureFO[A](f: Z[ThisEffect] => A !! ThisEffect): A !! ThisEffect = new Impure(effectId, f)

  final def impureHO[U <: ThisEffect] = new ImpureHOApply[U]
  final class ImpureHOApply[U <: ThisEffect]:
    def apply[A](f: Z[U] => A !! U): A !! U = new Impure(effectId, f)

  final def pure[A](a: A): A !! ThisEffect = !!.pure(a)

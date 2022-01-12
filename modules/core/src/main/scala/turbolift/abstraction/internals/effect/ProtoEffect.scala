package turbolift.abstraction.internals.effect
import turbolift.abstraction.{!!, Signature}
import turbolift.abstraction.ComputationCases.Impure


trait ProtoEffect[Z <: Signature] extends HasEffectId.Unsealed with Signature:
  final override type !@![+A, U] = A !! U

  final def impure[A, U <: ThisEffect](f: (z: Z with Signature.Apply[U]) => z.!@![A, U]): A !! U = new Impure(effectId, f)

  final def pure[A](a: A): A !! ThisEffect = !!.pure(a)

package turbolift.internals.effect
import turbolift.{!!, Signature}
import turbolift.ComputationCases.Perform


private[turbolift] trait ProtoEffect[Z <: Signature] extends HasEffectId.Unsealed with Signature:
  final override type !@![+A, U] = A !! U

  final def perform[A, U <: ThisEffect](f: (z: Z & Signature.Apply[U]) => z.!@![A, U]): A !! U = new Perform(effectId, f)

  final def pure[A](a: A): A !! ThisEffect = !!.pure(a)

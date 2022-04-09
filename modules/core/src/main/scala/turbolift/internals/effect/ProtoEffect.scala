package turbolift.internals.effect
import turbolift.{!!, Signature}
import turbolift.ComputationCases.Operate


trait ProtoEffect[Z <: Signature] extends HasEffectId.Unsealed with Signature:
  final override type !@![+A, U] = A !! U

  final def operate[A, U <: ThisEffect](f: (z: Z & Signature.Apply[U]) => z.!@![A, U]): A !! U = new Operate(effectId, f)

  final def pure[A](a: A): A !! ThisEffect = !!.pure(a)

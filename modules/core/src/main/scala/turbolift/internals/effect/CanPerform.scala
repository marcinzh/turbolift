package turbolift.internals.effect
import turbolift.{!!, Signature}
import turbolift.ComputationCases.Perform


private[turbolift] trait CanPerform[Z <: Signature] extends Signature:
  final override type !@![+A, U] = A !! U

  final def perform[A, U <: ThisEffect](f: (z: Z & Signature.Apply[U]) => z.!@![A, U]): A !! U = new Perform(this, f)

  final def pure[A](a: A): A !! ThisEffect = !!.pure(a)

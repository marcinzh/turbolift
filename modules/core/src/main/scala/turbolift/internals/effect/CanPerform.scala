package turbolift.internals.effect
import turbolift.{!!, Signature}
import turbolift.{ComputationCases => CC}


trait CanPerform[Z <: Signature] extends Signature:
  type ThisSignature[U] = Z & Signature { type ThisEffect = U }

  /** Lifts an invocation of this [[turbolift.Signature Signature]]'s
   *  method into the [[turbolift.Computation Computation]] monad.
   */
  final inline def perform[A, U <: ThisEffect](inline f: (Z & Signature { type ThisEffect = U }) => A !! U): A !! U =
    CC.perform[A, U, Z & Signature { type ThisEffect = U }](this, f)

  final def performNoInline[A, U <: ThisEffect](f: (Z & Signature { type ThisEffect = U }) => A !! U): A !! U =
    CC.perform[A, U, Z & Signature { type ThisEffect = U }](this, z => f(z).asInstanceOf[A !! U])

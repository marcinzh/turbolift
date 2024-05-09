package turbolift.internals.effect
import turbolift.{!!, Signature}
import turbolift.internals.primitives.{ComputationCases => CC}


trait CanPerform[Z <: Signature] extends Signature:
  /** Lifts an invocation of this [[turbolift.Signature Signature]]'s
   *  method into the [[turbolift.Computation Computation]] monad.
   */
  final def perform[A, U <: ThisEffect](f: (Z & Signature { type ThisEffect = U }) => !![A, U]): A !! U = new CC.Perform(this, f)

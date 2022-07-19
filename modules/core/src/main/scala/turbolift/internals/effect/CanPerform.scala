package turbolift.internals.effect
import turbolift.{!!, Signature}
import turbolift.ComputationCases.Perform


trait CanPerform[Z <: Signature] extends Signature:
  final override type !@![+A, U] = A !! U

  /** Lifts an invocation of this [[turbolift.Signature Signature]]'s method into the [[turbolift.Computation Computation]] monad.
    *
    * It's purpose is similar to:
    * - `send` function form various effect systems (Eff monad, etc.)
    * - `serviceWith` from ZIO 1.x.
    * - `serviceWithZIO` from ZIO 2.x.
    */
  final def perform[A, U <: ThisEffect](f: (z: Z & Signature { type ThisEffect = U }) => z.!@![A, U]): A !! U = new Perform(this, f)

  /** Like `!!.pure(a)`, but with effect-set up-casted to `ThisEffect` */
  final def pure[A](a: A): A !! ThisEffect = !!.pure(a)

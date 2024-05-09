package turbolift.interpreter
import turbolift.!!
import turbolift.internals.primitives.{ComputationCases => CC}
import turbolift.interpreter.Void

/** Access to delimited continuation.
 *
 * Used from custom implementations of effect [[turbolift.interpreter.Interpreter Interpreter]].
 *
 * @tparam S local state
 * @tparam Q part of continuation's answer
 * @tparam F part of continuation's answer
 * @tparam V effect set
 */

final class Control[S, Q, F[+_], V](private[turbolift] val interp: Interpreter.Untyped):
  type Cont[A, U] = Continuation[A, F[Q], S, U]

  /** Captures the continuation.
   *
   * Unwinds the stack up to the end of the effect's scope.
   * The unwound part of the stack is reified as a [[Continuation]] object.
   */
  def capture[A, U](f: Cont[A, U] => F[Q] !! U): A !! U = CC.Capture(interp, f)


  /** Like [[capture]], except it also accesses the local state.
   *
   * Fusion of [[Local.get]] and [[capture]].
   */
  def captureGet[A, U](f: (Cont[A, U], S) => F[Q] !! U): A !! U = CC.Capture(interp, f)

  /** Unwind the stack until the end of the effect's scope.
   *
   *  Can be thought of as "invoking the continuation **zero times**",
   *  as in: `capture(_ => value)`.
   * 
   *  However, `abort(value)` also invokes finalization clauses during stack unwinding.
   */
  def abort(value: F[Q]): Nothing !! V = CC.Abort(interp, value)

  /** Changes scope of the effect.
   *
   * The scope of the effect is defined as either:
   * - The body of the innermost `delimit` call.
   * - The the whole scope of the effect's handler, if none of the above is found.
   */
  def delimit[A, U <: V](body: A !! U): F[A] !! U = delimitPut(body, Void.as[S])

  /** Like [[delimit]], but also replaces the local state.
   *
   * Upon exit of [[delimit]]'s body, the local state is restored to its original value.
   */
  def delimitPut[A, U <: V](body: A !! U, s: S): F[A] !! U = CC.Delimit(interp, body.untyped, s, null)
  
  /** Like [[delimitPut]], but modifies the local state, instead of replacing it. */
  def delimitModify[A, U <: V](body: A !! U, f: S => S): F[A] !! U = CC.Delimit(interp, body.untyped, Void.as[S], f)


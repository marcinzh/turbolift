package turbolift.interpreter
import turbolift.{!!, ComputationCases => CC}
import turbolift.interpreter.Void

/** Access to delimited continuation.
 *
 * This is a **Primitive Effect**, provided for implementing custom effects:
 *
 * 1. It's accessible only from within custom implementations of [[Interpreter]]s.
 *    Custom effects can invoke [[Control]]'s operations to implement their own operations.
 *
 * 2. It does not require a handler.
 *    Invoking [[Control]]'s operations does not manifest as a dependency.
 *
 * See also another primitive effect: [[Local]].
 *
 * @tparam S local state
 * @tparam Q part of continuation's answer
 * @tparam F part of continuation's answer
 * @tparam V effect set
 */

final class Control[S, Q, F[+_], V] private[interpreter] (private val interp: Interpreter.Untyped):
  type Cont[A, U] = Continuation[A, F[Q], S, U]

  /** Captures the continuation.
   *
   * Unwinds the stack up to the end of the effect's scope.
   * The unwound part of the stack is reified as a [[Continuation]] object.
   */
  def capture[A, U](f: Cont[A, U] => F[Q] !! U): A !! U = CC.intristic(_.intristicCapture(interp, f))


  /** Like [[capture]], except it also accesses the local state.
   *
   * Fusion of [[Local.get]] and [[capture]].
   */
  def captureGet[A, U](f: (Cont[A, U], S) => F[Q] !! U): A !! U = CC.intristic(_.intristicCaptureGet(interp, f))

  /** Unwind the stack until the end of the effect's scope.
   *
   *  Can be thought of as invoking the continuation **zero times**,
   *  as in: `capture(_ => value)`.
   * 
   *  However, `abort(value)` also invokes finalization clauses during stack unwinding.
   */
  def abort(value: F[Q]): Nothing !! V = CC.intristic(_.intristicAbort(interp, value))

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
  def delimitPut[A, U <: V](body: A !! U, s: S): F[A] !! U = CC.intristic(_.intristicDelimitPut(interp, body.untyped, s))
  
  /** Like [[delimitPut]], but modifies the local state, instead of replacing it. */
  def delimitModify[A, U <: V](body: A !! U, f: S => S): F[A] !! U = CC.intristic(_.intristicDelimitMod(interp, body.untyped, f))


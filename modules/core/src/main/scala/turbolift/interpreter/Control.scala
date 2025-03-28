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
 * @tparam S local state, same as `Local` of the corresponding interpreter.
 * @tparam Q effect set, same as `Unknown` of the corresponding interpreter.
 * @tparam F input, same as `From[+_]` of the corresponding interpreter.
 * @tparam G output, same as `To[+_]` of the corresponding interpreter.
 * @tparam L effect set, same as `Elim` of the corresponding interpreter.
 * @tparam N effect set, same as `Intro` of the corresponding interpreter.
 * @tparam M effect set, same as `Ambient` of the corresponding interpreter.
 */

final class Control[S, Q, F[+_], G[+_], L, N, M] private[interpreter] (private val prompt: Prompt):
  type Cont[A, U] = Continuation[A, G[Q], S, U]
  type Cont0[A, U] = Continuation[A, F[Q], Nothing, U]

  /** Captures the continuation.
   *
   * Unwinds the stack up to the end of the effect's scope.
   * The unwound part of the stack is reified as a [[Continuation]] object.
   */
  def capture[A, U <: M & N](f: Cont[A, U] => G[Q] !! U): A !! U = CC.intrinsic(_.intrinsicCapture(prompt, f, truncate = false))

  /** Captures the continuation.
   *
   * Like `capture`, but the continuation is truncated to exclude the prompt of this effect.
   */
  def capture0[A, U <: M](f: Cont0[A, U & L] => G[Q] !! (U & N)): A !! (U & N) = CC.intrinsic(_.intrinsicCapture(prompt, f, truncate = true))

  /** Like [[capture]], except it also accesses the local state.
   *
   * Fusion of [[Local.get]] and [[capture]].
   */
  def captureGet[A, U <: M & N](f: (Cont[A, U], S) => G[Q] !! U): A !! U = CC.intrinsic(_.intrinsicCaptureGet(prompt, f, truncate = false))

  /** Like [[capture0]], except it also accesses the local state.
   *
   * Fusion of [[Local.get]] and [[capture0]].
   */
  def capture0Get[A, U <: M & N](f: (Cont0[A, U], S) => F[Q] !! U): A !! U = CC.intrinsic(_.intrinsicCaptureGet(prompt, f, truncate = true))

  /** Unwind the stack until the end of the effect's scope.
   *
   *  Can be thought of as invoking the continuation **zero times**,
   *  as in: `capture(_ => value)`.
   * 
   *  However, `abort(value)` also invokes finalization clauses during stack unwinding.
   */
  def abort(value: G[Q]): Nothing !! (M & N) = CC.intrinsic(_.intrinsicAbort(prompt, value))

  /** Changes scope of the effect.
   *
   * The scope of the effect is defined as either:
   * - The body of the innermost `delimit` call.
   * - The the whole scope of the effect's handler, if none of the above is found.
   */
  def delimit[A, U <: M & N](body: F[A] !! U): G[A] !! U = delimitPut(body, Void.as[S])

  /** Like [[delimit]], but also replaces the local state.
   *
   * Upon exit of [[delimit]]'s body, the local state is restored to its original value.
   */
  def delimitPut[A, U <: M & N](body: F[A] !! U, s: S): G[A] !! U = CC.intrinsic(_.intrinsicDelimitPut(prompt, body.untyped, s))
  
  /** Like [[delimitPut]], but modifies the local state, instead of replacing it. */
  def delimitModify[A, U <: M & N](body: F[A] !! U, f: S => S): G[A] !! U = CC.intrinsic(_.intrinsicDelimitMod(prompt, body.untyped, f))

  /** Reinterpret a computation.
   *
   * Allows this effect's operations to be invoked inside its own interpreter.
   */
  def reinterpret[A, U <: M & N](body: A !! (L & U)): A !! U = CC.intrinsic(_.intrinsicReinterpret(body))

  /** Bypass current interpreter. */
  def shadow[A, U](using N <:< L)(body: A !! U): A !! U = CC.intrinsic(_.intrinsicShadow(prompt, body))

  /** Strips away the `Ambient` effect.
   *
   * Useful when we need a continuation to outlive the interpreter that captured it.
   */
  def strip[A, U <: N](body: A !! (M & U)): A !! U = body.asInstanceOf[A !! U]

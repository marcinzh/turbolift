package turbolift.internals.interpreter
import turbolift.!!
import turbolift.internals.primitives.{ComputationCases => CC}


/** Delimited continuation.
 *
 * Accessed from implementations of user-defined [[turbolift.internals.interpreter.Interpreter.Flow Flow]] interpreters.
 */

abstract class Control[-A, B, S, F[+_], U, V] extends Function1[A, F[B] !! V]:
  /** Current state of the effect.
   *
   * For stateless interpreters, this is always equal [[Void]].
   */
  def get: S

  /** Alias for unary [[resume]]. */
  final override def apply(a: A): F[B] !! V = resume(a)
  
  /** Alias for binary [[resume]]. */
  final def apply(a: A, s: S): F[B] !! V = resume(a, s)

  /** Resumes the continuation. */
  final def resume(a: A): F[B] !! V = resume(a, void)
  
  /** Like the unary [[resume]], but also with updating the state. */
  final def resume(a: A, s: S): F[B] !! V = CC.Resume(untyped, a, s)

  /** Handles given computation locally.
   * 
   * Aside from the result of handling, a fresh [[Control]] object is returned.
   * Subsequent resuming of `this` continuation, instead of the fresh one,
   * may undo actions that have been performed by other effects inside the `body`.
   */
  final def local[X](body: X !! U): (F[X], This) !! V = local(body, void)

  /** Like the unary [[local]], but also with updating the state. */
  final def local[X](body: X !! U, s: S): (F[X], This) !! V = CC.Local(untyped, body.untyped, s)

  private type This = Control[A, B, S, F, U, V]

  private final def untyped: Control.Untyped = asInstanceOf[Control.Untyped]
  private final def void: S = Void.as[S]


private[internals] object Control:
   type Untyped = Control[Any, Any, Any, [X] =>> X, Any, Any]

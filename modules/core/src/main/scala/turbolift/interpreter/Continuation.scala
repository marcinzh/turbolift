package turbolift.interpreter
import turbolift.{!!, ComputationCases => CC}
import turbolift.interpreter.Void

/** Delimited continuation obtained from [[Control.capture]].
 *
 * @tparam A input
 * @tparam B output
 * @tparam S local state
 * @tparam U effect set
 */

abstract class Continuation[A, B, S, U] extends Function1[A, B !! U]:
  /** Resumes the continuation. */
  final override def apply(a: A): B !! U = CC.intrinsic(_.intrinsicResume(this, a))

  /** Resumes the continuation, also updating the local state. */
  final def apply(a: A, s: S): B !! U = CC.intrinsic(_.intrinsicResumePut(this, a, s))

  /** Tupled version of binary [[apply]]. */
  final def tupled(a_s: (A, S)): B !! U = apply(a_s._1, a_s._2)

  final def resume(a: A): B !! Any = apply(a).downCast[Any]
  final def resume(a: A, s: S): B !! Any = apply(a, s).downCast[Any]

  private[turbolift] final def untyped: Continuation.Untyped = asInstanceOf[Continuation.Untyped]
  private[turbolift] final def cast[A2, B2, S2, U2]: Continuation[A2, B2, S2, U2] = asInstanceOf[Continuation[A2, B2, S2, U2]]


private[turbolift] object Continuation:
  type Untyped = Continuation[Any, Any, Any, Any]
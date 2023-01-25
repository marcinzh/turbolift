package turbolift.internals.interpreter
import turbolift.!!
import turbolift.internals.primitives.{ComputationCases => CC}


object Control:

  final class Proxy[U, V]:
    /** Execute `body` in the context, where the currently interpreted operation has been encountered. */
    final def escape[X](body: X !! U): X !! V = CC.Escape(null, body.untyped, null)


  private[internals] val Proxy = new Proxy[Any, Any]


  /** Delimited continuation.
   *
   * Accessed from implementations of user-defined [[turbolift.internals.interpreter.Interpreter.Flow Flow]] interpreters.
   */
  abstract class Flow[-A, B, S, F[+_], U, V] extends Function1[A, F[B] !! V]:
    /** Alias for unary [[resume]]. */
    final override def apply(a: A): F[B] !! V = resume(a)
    
    /** Alias for binary [[resume]]. */
    final def apply(a: A, s: S): F[B] !! V = resume(a, s)

    /** Tupled version of binary [[resume]]. */
    final def tupled(a_s: (A, S)): F[B] !! V = resume(a_s._1, a_s._2)

    /** Resumes the continuation. */
    final def resume(a: A): F[B] !! V = resume(a, void)
    
    /** Resumes the continuation, also updating the state. */
    final def resume(a: A, s: S): F[B] !! V = CC.Resume(untyped, a, s)

    /** Execute `body` in the context, where the currently interpreted operation has been encountered.
     *
     * Similar to `local`, but more lightweight,
     * as it doesn't modify scope of the effect.
     */
    final def escape[X](body: X !! U): (X, This) !! V = escape(body, void).map { case (a, b, _) => (a, b) }

    /** Like the unary [[escape]], but also with updating the state. */
    final def escape[X](body: X !! U, s: S): (X, This, S) !! V = CC.Escape(untyped, body.untyped, void)

    /** Handles given computation locally.
     *
     * Execute `body` in the context, where the currently interpreted operation has been encountered,
     * as if scope of the current effect was delimited by `body`.
     *
     * Returns a pair of:
     * 1. The result of handling of the effect, in the scope delimited by the `body`.
     * 2. A fresh `Control` object.
     *
     * The fresh `Control` should be used for subsequent resumption,
     * because it captures actions,
     * that may have been performed inside the `body` by **other effects**.
     *
     * The stale continuation (`this`) can be resumed too.
     * However, this introduces semantic fragility.
     * Such that, we may find that some of those **other effects** are undone,
     * even though we have just got a hold on the value computed with their participation.
     *
     * This alternative behavior is similar to the known problem of Monad Transformers: "catch resets state".
     */
    final def local[X](body: X !! U): (F[X], This) !! V = local(body, void).map { case (a, b, _) => (a, b) }

    /** Like the unary [[local]], but also with updating the state. */
    final def local[X](body: X !! U, s: S): (F[X], This, S) !! V = CC.Local(untyped, body.untyped, s)

    private type This = Flow[A, B, S, F, U, V]

    private final def untyped: FlowUntyped = asInstanceOf[FlowUntyped]
    private final def void: S = Void.as[S]


  private[internals] type FlowUntyped = Flow[Any, Any, Any, [X] =>> X, Any, Any]
  private[internals] type ProxyUntyped = Proxy[Any, Any]

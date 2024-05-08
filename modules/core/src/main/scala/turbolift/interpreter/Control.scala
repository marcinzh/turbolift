package turbolift.interpreter
import turbolift.!!
import turbolift.internals.primitives.{ComputationCases => CC}


/** Access to delimited continuation.
 *
 * Used from custom implementations of effect [[turbolift.interpreter.Interpreter Interpreter]].
 *
 * @tparam A input of this continuation
 * @tparam B part of the result of this continuation
 * @tparam S state of the current effect
 * @tparam F part of the result of this continuation
 * @tparam U effect set for higher order operations
 * @tparam V part of the result of this continuation 
 */
abstract class Control[-A, B, S, F[+_], U, V] extends Function1[A, F[B] !! V]:
  /** Resumes the continuation. */
  final override def apply(a: A): F[B] !! V = apply(a, void)

  /** Resumes the continuation, also updating the state. */
  final def apply(a: A, s: S): F[B] !! V = CC.Resume(untyped, a, s)

  /** Tupled version of binary [[apply]]. */
  final def tupled(a_s: (A, S)): F[B] !! V = apply(a_s._1, a_s._2)

  //-------------

  //@#@TODO doc
  final def resume(a: A): F[B] !! Any = resume(a, void)
  final def resume(a: A, s: S): F[B] !! Any = CC.Resume(untyped, a, s)
  final def resumeTupled(a_s: (A, S)): F[B] !! Any = resume(a_s._1, a_s._2)

  //-------------

  //@#@TODO doc
  final def abort(value: F[B]): F[B] !! V = CC.Abort(untyped, value)


  //@#@TODO doc
  /** Execute `body` in the context, where this continuation has been created.
   * 
   * ...
   */
  final def escape[X](body: X !! U): (X, This) !! V = escape(body, void).map { case (x, k, _) => (x, k) }

  /** Like the unary [[escape]], but also with updating the state. */
  final def escape[X](body: X !! U, s: S): (X, This, S) !! V = CC.Escape(untyped, body.untyped, s)

  //@#@TODO doc
  final def escapeAndResume(body: A !! U): F[B] !! V = escapeAndResume(body, void)

  //@#@TODO doc
  //@#@OPTY fuse
  final def escapeAndResume(body: A !! U, s: S): F[B] !! V = escape(body, s).flatMap { case (a, k, _) => k(a) }

  //@#@TODO doc
  final def escapeAndForget(body: F[B] !! U): F[B] !! V = escapeAndForget(body, void)

  //@#@TODO doc
  //@#@OPTY fuse
  final def escapeAndForget(body: F[B] !! U, s: S): F[B] !! V = escape(body, s).map(_._1)

  //@#@TODO doc
  //@#@OPTY fuse
  final def escapeAndAbort(body: F[B] !! U): F[B] !! V = escape(body, void).flatMap { case (bb, k, _) => k.abort(bb) }


  //@#@TODO doc
  /** Handles given computation locally.
   *
   * Execute `body` in the context, where this continuation has been created,
   * as if scope of the currently interpreted effect was delimited by `body`.
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
  final def delimit[X](body: X !! U): (F[X], This) !! V = delimit(body, void).map { case (xx, k, _) => (xx, k) }

  /** Like the unary [[delimit]], but also with updating the state. */
  final def delimit[X](body: X !! U, s: S): (F[X], This, S) !! V = CC.Delimit(untyped, body.untyped, s)


  //@#@TODO doc
  final def delimitAndResume[X](body: X !! U)(using ev: F[X] <:< A): F[B] !! V = delimitAndResume(body, void)

  //@#@TODO doc
  //@#@OPTY fuse
  final def delimitAndResume[X](body: X !! U, s: S)(using ev: F[X] <:< A): F[B] !! V =
    delimit(body, s).flatMap { case (xx, k, _) => k(ev(xx)) }

  //@#@TODO doc
  final def delimitAndForget(body: B !! U): F[B] !! V = delimitAndForget(body, void)

  //@#@TODO doc
  //@#@OPTY fuse
  final def delimitAndForget(body: B !! U, s: S): F[B] !! V = delimit(body, s).map(_._1)

  private type This = Control[A, B, S, F, U, V]

  private final def untyped: Control.Untyped = asInstanceOf[Control.Untyped]
  private final def void: S = Void.as[S]


object Control:
  private[turbolift] type Untyped = Control[Any, Any, Any, [X] =>> X, Any, Any]

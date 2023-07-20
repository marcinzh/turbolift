package turbolift.internals.extensions
import scala.util.{Try, Success, Failure}
import turbolift.{!!, Handler}
import turbolift.typeclass.ExtendTuple
import turbolift.internals.auxx.CanPipe


/** No need to use this trait directly, because it's inherited by [[turbolift.Handler Handler]]'s companion object. */
/*private[turbolift]*/ trait HandlerExtensions:
  extension [F[+_], S, L, N](thiz: Handler[F, (_, S), L, N])
    /** Alias for [[dropState]]. */
    def eval: Handler[F, [X] =>> X, L, N] = dropState

    /** Alias for [[justState]]. */
    def exec: Handler[F, [_] =>> S, L, N] = justState

    /** Transforms this handler, by dropping the first element of its `Tuple2` result. */
    def justState: Handler[F, [_] =>> S, L, N] = thiz.map([A] => (pair: (A, S)) => pair._2)

    /** Transforms this handler, by dropping the second element of its `Tuple2` result. */
    def dropState: Handler[F, [X] =>> X, L, N] = thiz.map([A] => (pair: (A, S)) => pair._1)

    /** Transforms this handler, by mapping the second element of its `Tuple2` result. */
    def mapState[S2](f: S => S2): Handler[F, (_, S2), L, N] =
      thiz.map([A] => (pair: (A, S)) =>
        val (a, s) = pair
        (a, f(s))
      )

    /** Like [[mapState]], but the mapping function is effectful. */
    def flatMapState[S2, U](f: S => S2 !! U): Handler[F, (_, S2), L, (N & U)] =
      thiz.flatMap([A] => (pair: (A, S)) =>
        val (a, s) = pair
        f(s).map((a, _))
      )

    /** Like [[flatMapState]], but the mapping is executed for its effects only. */
    def flatTapState[S2, U](f: S => Unit !! U): Handler[F, (_, S), L, (N & U)] =
      thiz.flatTap([A] => (pair: (A, S)) =>
        val (_, s) = pair
        f(s)
      )

    /** Composes 2 **independent** handlers, also flattening their nested `Tuple2` results.
     *
     * {{{
     * ((_, S1), S2) ~> (_, (S1, S2))
     * ((_, S1, S2), S3) ~> (_, (S1, S2, S3))
     * ((_, S1, S2, S3), S4) ~> (_, (S1, S2, S3, S4))
     * ...
     * }}}
     */

    def ***![S2, S3, L2, N2](that: Handler.FromId[(_, S2), L2, N2])(using ET: ExtendTuple[S, S2, S3]): Handler[F, (_, S3), L & L2, N & N2] =
      thiz.composeWith(that).map([A] => (pairs: ((A, S), S2)) =>
        val ((a, s), s2) = pairs
        (a, ET.extendTuple(s, s2))
      )


  extension [F[+_], L, N](thiz: Handler[F, Option, L, N])
    /** Transforms this handler, by mapping its `Option` result to `Vector`. */
    def toVector: Handler[F, Vector, L, N] =
      thiz.map([A] => (result: Option[A]) => result.toVector)
    
    /** Transforms this handler, by mapping its `Option` result to `Either`. */
    def toEither[E](e: => E): Handler[F, Either[E, _], L, N] =
      thiz.map([A] => (result: Option[A]) => result.toRight(e))

    /** Transforms this handler, by mapping its `Option` result to `Try`. */
    def toTry(e: => Throwable): Handler[F, Try, L, N] =
      thiz.map([A] => (result: Option[A]) => result.fold[Try[A]](Failure(e))(Success(_)))

    /** Transforms this handler, by deconstructing its `Option` result. */
    def getOrElse(default: => Nothing): Handler[F, [X] =>> X, L, N] =
      thiz.map([A] => (result: Option[A]) => result.getOrElse(default))

    /** Transforms this handler, by deconstructing its `Option` result. */
    def getOrDie(message: => String): Handler[F, [X] =>> X, L, N] =
      getOrElse(sys.error(message))

    /** Transforms this handler, by deconstructing its `Option` result. */
    def unsafeGet: Handler[F, [X] =>> X, L, N] =
      thiz.map([A] => (result: Option[A]) => result.get)

    /** Composes 2 **independent** handlers, also flattening their nested `Option` results.
     *
     * {{{
     * Option[Option[_]] ~> Option[_]
     * }}}
     */
    @annotation.targetName("flattenOptions")
    def |||![L2, N2](that: Handler.FromId[Option, L2, N2]): Handler[F, Option, L & L2, N & N2] =
      thiz.composeWith(that).map([A] => (options: Option[Option[A]]) => options.flatten)


  extension [F[+_], E, L, N](thiz: Handler[F, Either[E, _], L, N])
    /** Transforms this handler, by mapping its `Either` result to `Option`. */
    def toOption: Handler[F, Option, L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.toOption)

    /** Transforms this handler, by mapping its `Either` result to `Try`. */
    def toTry(implicit ev: E <:< Throwable): Handler[F, Try, L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.toTry)

    /** Transforms this handler, by deconstructing its `Either` result. */
    def getOrElse(default: E => Nothing): Handler[F, [X] =>> X, L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.fold(default, x => x))

    /** Transforms this handler, by deconstructing its `Either` result. */
    def getOrDie(message: E => String): Handler[F, [X] =>> X, L, N] =
      getOrElse(e => sys.error(message(e)))

    /** Transforms this handler, by mapping the `Left` branch of its `Either` result. */
    def mapLeft[E2](f: E => E2): Handler[F, Either[E2, _], L, N] =
      thiz.map([A] => (result: Either[E, A]) => result.swap.map(f).swap)

    /** Like [[mapLeft]], but the mapping function is effectful. */
    def flatMapLeft[E2, U](f: E => E2 !! U): Handler[F, Either[E2, _], L, (N & U)] =
      thiz.flatMap([A] => (result: Either[E, A]) => result match
        case Left(e) => f(e).map(Left(_))
        case Right(a) => !!.pure(Right(a))
      )

    /** Like [[flatMapLeft]], but the mapping is executed for its effects only. */
    def flatTapLeft[U](f: E => Unit !! U): Handler[F, Either[E, _], L, (N & U)] =
      thiz.flatTap([A] => (result: Either[E, A]) => result match
        case Left(e) => f(e)
        case _ => !!.unit
      )

    /** Composes 2 **independent** handlers, also flattening their nested `Either` results.
     *
     * {{{
     * Either[E2, Either[E1, _]] ~> Either[E1 | E2, _]
     * }}}
     */
    @annotation.targetName("flattenEithers")
    def |||![E2, L2, N2](that: Handler.FromId[Either[E2, _], L2, N2]): Handler[F, Either[E | E2, _], L & L2, N & N2] =
      thiz.composeWith(that).map([A] => (eithers: Either[E2, Either[E, A]]) => eithers.flatten: Either[E2 | E, A])

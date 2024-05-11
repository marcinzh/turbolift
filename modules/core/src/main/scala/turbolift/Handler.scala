package turbolift
import scala.util.{Try, Success, Failure}
import turbolift.internals.auxx.{CanPartiallyHandle, CanPipe}
import turbolift.internals.auxx.IdConst._
import turbolift.interpreter.Interpreter
import turbolift.internals.primitives.{ComputationCases => CC}
import turbolift.typeclass.ExtendTuple

/** Used to delimit scope of effect(s).
 *
 *  Handler transforms a [[Computation]], by discharging some or all of its requested effects,
 *  and (optionally) introducing handlers own dependencies to it (set of effects used to create the handler).
 *  {{{
 *  val myComputation2 = myComputation1.handleWith(myHandler)
 *  }}}
 *  
 *  Handlers can be obtained by:
 *  - implementing an [[interpreter.Interpreter Interpreter]] for an [[Effect]],
 *  and then calling `toHandler` method on it.
 *  - transforming a preexisting handler, e.g. `val myHandler2 = myHandler1.map(...)`
 *  - composing 2 preexisting handlers, e.g. `val myHandler3 = myHandler1 &&&! myHandler2`
 *  
 *  Compositon of 2 handlers is always *sequential*: the operands are applied in left to right order.
 *  
 *  The `From[_]` type parameter can only have 2 forms:
 *  - type-level identity function: `[X] =>> X`
 *  - type-level constant function: `[_] =>> C`
 *  
 *  The `To[_]` type parameter has no restrictions, but often happens to be identity.
 *  
 *  @tparam From Type constructor, indicating what computation's result types can be accepted by this handler as input.
 *  @tparam To Type constructor (e.g. `Option[_]`), in which the computation's result is wrapped, after application of this handler.
 *  @tparam Elim Type-level set of effects, that this handler __eliminates__ from the computation.
 *  @tparam Intro Type-level set of effects, that this handler __introduces__ into the computation. This is often an empty set, expressed as `Any`.
 */

sealed trait Handler[From[+_], To[+_], Elim, Intro]:
  private [turbolift] def doHandle[A, U](comp: From[A] !! (U & Elim)): To[A] !! (U & Intro)

  /** Applies this handler to given computation.
    *
    * Equivalent of [[Computation]]'s `handleWith(this)`
    */
  final def handle[V] = new HandleSyntax[V]
  final class HandleSyntax[V]:
    def apply[A, W](comp: From[A] !! W)(implicit ev: CanPartiallyHandle[V, W, Elim]): To[A] !! (V & Intro) =
      doHandle[A, V](ev(comp))

  /** Transforms this handler, by applying a post-processing function to its result`.
    *
    * a.k.a Natural Transformation.
    */
  final def mapK[To2[+_]](f: [X] => To[X] => To2[X]): Handler[From, To2, Elim, Intro] =
    mapEffK([X] => (xx: To[X]) => !!.pure(f(xx)))

  /** Like [[mapK]], but the post-processing of `To[_]` can also introduce effects.
    *
    * Those effects are then absorbed by the new handler into the effects it introduces.
    */
  final def mapEffK[To2[+_], V](f: [X] => To[X] => To2[X] !! V): Handler[From, To2, Elim, Intro & V] =
    HandlerCases.Mapped[From, To, To2, Elim, Intro, V](this, f)

  /** Like [[mapEffK]], but the post-processing is executed for its effects only.
    *
    * This handler's `To[_]` remains unchanged.
    */
  final def tapEffK[V](f: [X] => To[X] => Unit !! V): Handler[From, To, Elim, Intro & V] =
    mapEffK([X] => (xx: To[X]) => f(xx).as(xx))

  /** Transforms this handler, by applying a pre-processing function to its input`. */
  final def contraMapK[From2[+_]](f: [X] => From2[X] => From[X]): Handler[From2, To, Elim, Intro] =
    contraMapEffK([X] => (xx: From2[X]) => !!.pure(f(xx)))

  /** Like [[contraMapK]], but the pre-processing of `From[_]` can also introduce effects.
    *
    * Those effects are then absorbed by the new handler into the effects it introduces.
    */
  final def contraMapEffK[From2[+_], V](f: [X] => From2[X] => From[X] !! V): Handler[From2, To, Elim, Intro & V] =
    HandlerCases.ContraMapped[From2, From, To, Elim, V, Intro](f, this)

  /** Like [[contraMapEffK]], but the pre-processing is executed for its effects only.
    *
    * This handler's `From[_]` remains unchanged.
    */
  final def contraTapEffK[V](f: [X] => From[X] => Unit !! V): Handler[From, To, Elim, Intro & V] =
    contraMapEffK([X] => (xx: From[X]) => f(xx).as(xx))

  /** Composes 2 **independent** handlers.
    *
    * Independence of handlers means, that effects __eliminated__ by one of the handlers,
    * do not overlap with effects __introduced__ by the other.
    *
    * Independence of 2 handlers guarantees, that it's also valid to compose them in the opposite order.
    * However, nesting order of their `To[_]`s would also be reversed.
    */
  final def composeWith[From2[+_], To2[+_], Elim2, Intro2](that: Handler[From2, To2, Elim2, Intro2])(using ev: CanPipe[To, From2]) =
    HandlerCases.Piped[From, From2, To, To2, Elim, Elim2, Intro, Intro2, Any](this, that).self
  
  /** Composes 2 **fully dependent** handlers.
    *
    * Assumes that **all** effects introduced by this handler, are eliminated by `that` handler.
    */
  final def provideWith[From2[+_], To2[+_], Intro2](that: Handler[From2, To2, Intro, Intro2])(using ev: CanPipe[To, From2]) =
    HandlerCases.Piped[From, From2, To, To2, Elim, Any, Any, Intro2, Intro](Handler.this, that).self

  /** Composes 2 **partially dependent** handlers.
    *
    * Assumes that **some of** effects introduced by this handler, are eliminated by `that` handler.
    */
  final def partiallyProvideWith[Remains >: Intro] = new PartiallyProvideWithSyntax[Remains]
  final class PartiallyProvideWithSyntax[Remains >: Intro]:
    def apply[From2[+_], To2[+_], Elim2 >: Intro, Intro2](that: Handler[From2, To2, Elim2, Intro2])(using ev: CanPipe[To, From2]) =
     HandlerCases.Piped[From, From2, To, To2, Elim, Any, Remains, Intro2, Elim2](upCastIntro[Remains & Elim2], that).self

  /** Specializes this handler, making it applicable only to computations returning `C` type.
    *
    * Removes universal quantification of the handler over its input type. 
    */
  final def project[C](using CanPipe[[_] =>> C, From]): Handler[[_] =>> C, [_] =>> To[C], Elim, Intro] =
    Handler.identity[[_] =>> C].composeWith(this)

  /** Alias for [[provideWith]]. */
  final def %%%![From2[+_], To2[+_], Intro2](that: Handler[From2, To2, Intro, Intro2])(using ev: CanPipe[To, From2]) = provideWith(that)

  /** Alias for [[partiallyProvideWith]]. */
  final def %%![Remains >: Intro] = partiallyProvideWith[Remains]

  private[turbolift] final def upCastIntro[T >: Intro] = asInstanceOf[Handler[From, To, Elim, T]]

  private[turbolift] final def self: Handler[From, To, Elim, Intro] = this

  /** Alias for [[composeWith]]. */
  final def &&&![From2[+_], To2[+_], Elim2, Intro2](that: Handler[From2, To2, Elim2, Intro2])(using ev: CanPipe[To, From2]) = this.composeWith(that)

  /** Transforms this handler, by discarding its result. */
  final def void: Handler[From, [X] =>> Unit, Elim, Intro] = mapK([X] => (_: To[X]) => ())


object Handler:
  /** Transforms a computation of a handler, into a new handler.
   *
   *  Useful for effectful creation of handlers.
   *  Effects requested to create the handler are absorbed by the handler itself,
   *  into its own (additional) dependencies.
   */
  def flatHandle[F[+_], G[+_], L, N1, N2](h: Handler[F, G, L, N1] !! N2): Handler[F, G, L, N1 & N2] = HandlerCases.FlatHandled(h)

  def identity[F[+_]]: Handler[F, F, Any, Any] = HandlerCases.Identity[F]()


  //---------- Extensions ----------


  extension [F[+_], S, L, N](thiz: Handler[F, (_, S), L, N])
    /** Alias for [[dropState]]. */
    def eval: Handler[F, Identity, L, N] = dropState

    /** Alias for [[justState]]. */
    def exec: Handler[F, Const[S], L, N] = justState

    /** Transforms this handler, by dropping the first element of its `Tuple2` result. */
    def justState: Handler[F, Const[S], L, N] = thiz.mapK([A] => (pair: (A, S)) => pair._2)

    /** Transforms this handler, by dropping the second element of its `Tuple2` result. */
    def dropState: Handler[F, Identity, L, N] = thiz.mapK([A] => (pair: (A, S)) => pair._1)

    /** Transforms this handler, by mapping the second element of its `Tuple2` result. */
    def mapState[S2](f: S => S2): Handler[F, (_, S2), L, N] =
      thiz.mapK([A] => (pair: (A, S)) =>
        val (a, s) = pair
        (a, f(s))
      )

    /** Like [[mapState]], but the mapping function is effectful. */
    def flatMapState[S2, U](f: S => S2 !! U): Handler[F, (_, S2), L, (N & U)] =
      thiz.mapEffK([A] => (pair: (A, S)) =>
        val (a, s) = pair
        f(s).map((a, _))
      )

    /** Like [[flatMapState]], but the mapping is executed for its effects only. */
    def flatTapState[S2, U](f: S => Unit !! U): Handler[F, (_, S), L, (N & U)] =
      thiz.tapEffK([A] => (pair: (A, S)) =>
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

    def ***![S2, S3, L2, N2](that: Handler[Identity, (_, S2), L2, N2])(using ET: ExtendTuple[S, S2, S3]): Handler[F, (_, S3), L & L2, N & N2] =
      thiz.composeWith(that).mapK([A] => (pairs: ((A, S), S2)) =>
        val ((a, s), s2) = pairs
        (a, ET.extendTuple(s, s2))
      )


  extension [F[+_], L, N](thiz: Handler[F, Option, L, N])
    /** Transforms this handler, by mapping its `Option` result to `Vector`. */
    def toVector: Handler[F, Vector, L, N] =
      thiz.mapK([A] => (result: Option[A]) => result.toVector)
    
    /** Transforms this handler, by mapping its `Option` result to `Either`. */
    def toEither[E](e: => E): Handler[F, Either[E, _], L, N] =
      thiz.mapK([A] => (result: Option[A]) => result.toRight(e))

    /** Transforms this handler, by mapping its `Option` result to `Try`. */
    def toTry(e: => Throwable): Handler[F, Try, L, N] =
      thiz.mapK([A] => (result: Option[A]) => result.fold[Try[A]](Failure(e))(Success(_)))

    /** Transforms this handler, by deconstructing its `Option` result. */
    def getOrElse(default: => Nothing): Handler[F, Identity, L, N] =
      thiz.mapK([A] => (result: Option[A]) => result.getOrElse(default))

    /** Transforms this handler, by deconstructing its `Option` result. */
    def getOrDie(message: => String): Handler[F, Identity, L, N] =
      getOrElse(sys.error(message))

    /** Transforms this handler, by deconstructing its `Option` result. */
    def unsafeGet: Handler[F, Identity, L, N] =
      thiz.mapK([A] => (result: Option[A]) => result.get)

    /** Composes 2 **independent** handlers, also flattening their nested `Option` results.
     *
     * {{{
     * Option[Option[_]] ~> Option[_]
     * }}}
     */
    @annotation.targetName("flattenOptions")
    def |||![L2, N2](that: Handler[Identity, Option, L2, N2]): Handler[F, Option, L & L2, N & N2] =
      thiz.composeWith(that).mapK([A] => (options: Option[Option[A]]) => options.flatten)


  extension [F[+_], E, L, N](thiz: Handler[F, Either[E, _], L, N])
    /** Transforms this handler, by mapping its `Either` result to `Option`. */
    def toOption: Handler[F, Option, L, N] =
      thiz.mapK([A] => (result: Either[E, A]) => result.toOption)

    /** Transforms this handler, by mapping its `Either` result to `Try`. */
    def toTry(implicit ev: E <:< Throwable): Handler[F, Try, L, N] =
      thiz.mapK([A] => (result: Either[E, A]) => result.toTry)

    /** Transforms this handler, by deconstructing its `Either` result. */
    def getOrElse(default: E => Nothing): Handler[F, Identity, L, N] =
      thiz.mapK([A] => (result: Either[E, A]) => result.fold(default, x => x))

    /** Transforms this handler, by deconstructing its `Either` result. */
    def getOrDie(message: E => String): Handler[F, Identity, L, N] =
      getOrElse(e => sys.error(message(e)))

    /** Transforms this handler, by mapping the `Left` branch of its `Either` result. */
    def mapLeft[E2](f: E => E2): Handler[F, Either[E2, _], L, N] =
      thiz.mapK([A] => (result: Either[E, A]) => result.swap.map(f).swap)

    /** Like [[mapLeft]], but the mapping function is effectful. */
    def flatMapLeft[E2, U](f: E => E2 !! U): Handler[F, Either[E2, _], L, (N & U)] =
      thiz.mapEffK([A] => (result: Either[E, A]) => result match
        case Left(e) => f(e).map(Left(_))
        case Right(a) => !!.pure(Right(a))
      )

    /** Like [[flatMapLeft]], but the mapping is executed for its effects only. */
    def flatTapLeft[U](f: E => Unit !! U): Handler[F, Either[E, _], L, (N & U)] =
      thiz.tapEffK([A] => (result: Either[E, A]) => result match
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
    def |||![E2, L2, N2](that: Handler[Identity, Either[E2, _], L2, N2]): Handler[F, Either[E | E2, _], L & L2, N & N2] =
      thiz.composeWith(that).mapK([A] => (eithers: Either[E2, Either[E, A]]) => eithers.flatten: Either[E2 | E, A])



private[turbolift] object HandlerCases:
  final case class Identity[F[+_]]() extends Handler[F, F, Any, Any]:
    override def doHandle[A, U](comp: F[A] !! U): F[A] !! U = comp

  final case class Primitive[From[+_], To[+_], Elim, Intro](
    interpreter: Interpreter.Apply[From, To, Elim, Intro]
  ) extends Handler[From, To, Elim, Intro]:
    override def doHandle[A, U](comp: From[A] !! (U & Elim)): To[A] !! (U & Intro) =
      new CC.Handle[A, U, From, To, Elim, Intro](comp, this)

  final case class Piped[From1[+_], From2[+_], To1[+_], To2[+_], Elim1, Elim2, Intro1, Intro2, Hidden](
    first: Handler[From1, To1, Elim1, Intro1 & Hidden],
    second: Handler[From2, To2, Elim2 & Hidden, Intro2],
  )(using ev: CanPipe[To1, From2]
  ) extends Handler[[X] =>> From1[From2[X]], [X] =>> To2[To1[X]], Elim1 & Elim2, Intro1 & Intro2]:
    override def doHandle[A, U](comp: From1[From2[A]] !! (U & Elim1 & Elim2)): To2[To1[A]] !! (U & Intro1 & Intro2) =
      second.doHandle[To1[A], U & Intro1](
        first.doHandle[From2[A], U & Elim2](comp)
        .map(ev.fit)
      )

  final case class Mapped[From[+_], To1[+_], To2[+_], Elim, Intro1, Intro2](
    that: Handler[From, To1, Elim, Intro1],
    fun: [X] => To1[X] => To2[X] !! Intro2,
  ) extends Handler[From, To2, Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: From[A] !! (U & Elim)): To2[A] !! (U & Intro1 & Intro2) =
      that.doHandle[A, U](comp).flatMap(fun(_))

  final case class ContraMapped[From1[+_], From2[+_], To[+_], Elim, Intro1, Intro2](
    fun: [X] => From1[X] => From2[X] !! Intro1,
    that: Handler[From2, To, Elim, Intro2],
  ) extends Handler[From1, To, Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: From1[A] !! (U & Elim)): To[A] !! (U & Intro1 & Intro2) =
      that.doHandle[A, U & Intro1](comp.flatMap(fun(_)))

  final case class FlatHandled[From[+_], To[+_], Elim, Intro1, Intro2](
    that: Handler[From, To, Elim, Intro1] !! Intro2,
  ) extends Handler[From, To, Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: From[A] !! (U & Elim)): To[A] !! (U & Intro1 & Intro2) =
      that.flatMap(_.doHandle[A, U](comp))

package turbolift
import turbolift.internals.aux.CanPartiallyHandle
import turbolift.internals.extensions.HandlerExtensions
import turbolift.internals.interpreter.Interpreter

/** Handler is an object used to transform a [[Computation]], by discharging some or all of its requested effects.
 *
 *  For example, having:
 *  {{{
 *  val myComputation2 = myComputation1.handleWith(myHandler)
 *  }}}
 *  ...then, `myComputation2` will have the type of `myComputation1`, modified as follows:
 *  - [[Elim]] effects will be **removed** from the set of requested effects.
 *  - [[Intro]] effects (if any) will be **inserted** to the set of requested effects.
 *  - The result type `A`, will be transformed into `Result[A]`.
 *  
 *  Handlers can be obtained in 3 ways:
 *  - By implementing an [[internals.interpreter.Interpreter Interpreter]] for an [[Effect]], and then transforming it into a [[Handler]].
 *  - By transforming a preexisting handler, e.g: `val myHandler2 = myHandler1.map(...)`
 *  - By composing 2 preexisting handlers, e.g: `val myHandler3 = myHandler1 &&&! myHandler2`
 *  
 *  @tparam Result Type constructor (e.g. `Option[_]`), in which the computation's result is wrapped, after application of this handler. This is often an identity.
 *  @tparam Elim Type-level set of effects, expressed as an intersection type, that this handler __eliminates__ from the computation.
 *  @tparam Intro Type-level set of effects, expressed as an intersection type, that this handler __introduces__ into the computation. This is often an empty set, expressed as `Any`.
 */

sealed trait Handler[Result[+_], Elim, Intro]:
  private [turbolift] def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro)

  /** Applies this handler to given computation.
    *
    * Equivalent of [[Computation]]'s `handleWith(this)`
    */
  final def handle[V] = new HandleApply[V]
  final class HandleApply[V]:
    def apply[A, W](comp: A !! W)(implicit ev: CanPartiallyHandle[V, W, Elim]): Result[A] !! (V & Intro) =
      doHandle[A, V](ev(comp))

  /** Runs given computation, provided that all of its effects can be discharged by this handler.
    * 
    * Equivalent of [[Computation]]'s `handleWith(this)` followed by `run`
    */
  final def run[A](comp: A !! Elim)(implicit ev: Intro =:= Any): Result[A] = handle[Any](comp).run

  /** Composes this handler with a post-processing function, applied to this handler's `Result[_]`.
    *
    * a.k.a Natural Transformation.
    */
  final def map[NewResult[+_]](f: [X] => Result[X] => NewResult[X]): Handler[NewResult, Elim, Intro] =
    HandlerCases.Mapped[Result, NewResult, Elim, Intro](this, f)

  /** Like `map`, but the post-processing of `Result[_]` can also introduce effects.
    *
    * Those effects are then absorbed by the new handler into the effects it introduces.
    */
  final def flatMap[NewResult[+_], V](f: [X] => Result[X] => NewResult[X] !! V): Handler[NewResult, Elim, Intro & V] =
    HandlerCases.FlatMapped[Result, NewResult, Elim, Intro, V](this, f)

  /** Like `flatMap`, but the post-processing is executed for its effects only.
    *
    * This handler's `Result[_]` remains unchanged.
    */
  final def flatTap[V](f: [X] => Result[X] => Unit !! V): Handler[Result, Elim, Intro & V] =
    HandlerCases.FlatTapped[Result, Elim, Intro, V](this, f)

  /** Composes 2 **independent** handlers sequentially. This handler is applied first.
    *
    * Independence of handlers means, that effects __eliminated__ by one of the handlers, do not overlap with effects __introduced__ by the other.
    *
    * Independence of 2 handlers guarantees, that it is also valid to compose them in the opposite order.
    * However, nesting order of their `Result[_]`s would also be reversed.
    */
  final def composeWith[ThatResult[+_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, ThatElim, Intro, ThatIntro, Any](this, that).self
  
  /** Composes 2 **fully dependent** handlers sequentially. This handler is applied first.
    *
    * Assumes that **all** effects introduced by this handler, are eliminated by `that` handler.
    */
  final def provideWith[ThatResult[+_], ThatIntro](that: Handler[ThatResult, Intro, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, Any, Any, ThatIntro, Intro](Handler.this, that).self

  /** Composes 2 **partially dependent** handlers sequentially. This handler is applied first.
    *
    * Assumes that **some of** effects introduced by this handler, are eliminated by `that` handler.
    */
  final def partiallyProvideWith[Remains >: Intro] = new PartiallyProvideWithApply[Remains]
  class PartiallyProvideWithApply[Remains >: Intro]:
    def apply[ThatResult[+_], ThatElim >: Intro, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
     HandlerCases.Composed[Result, ThatResult, Elim, Any, Remains, ThatIntro, ThatElim](upCastIntro[Remains & ThatElim], that).self

  private[turbolift] final def upCastIntro[T >: Intro] = asInstanceOf[Handler[Result, Elim, T]]

  private[turbolift] final def self: Handler[Result, Elim, Intro] = this

  /** Alias for `composeWith`. */
  final def &&&![ThatResult[+_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) = this.composeWith(that)

  /** Maps `Result[_]` to `Unit`. */
  final def void: Handler[[X] =>> Unit, Elim, Intro] = map([X] => (_: Result[X]) => ())


/** Defines convenience extensions and type aliases for [[Handler]]. */
object Handler extends HandlerExtensions:

  /** Alias for handler, whose `Result[_]` is type-level identity. */
  type Id[Elim, Intro] = Handler[[X] =>> X, Elim, Intro]

  /** Alias for handler, whose `Result[_]` is a type-level constant function. */
  type Const[Result, Elim, Intro] = Handler[[X] =>> Result, Elim, Intro]

  /** Alias for handler that has no dependencies (introduces no new effects). */
  type Free[Result[+_], Elim] = Handler[Result, Elim, Any]

  /** Alias for handler, that is both [[Free]] and [[Id]]. */
  type FreeId[Elim] = Handler[[X] =>> X, Elim, Any]

  /** Alias for handler, that is both [[Free]] and [[Const]]. */
  type FreeConst[Result, Elim] = Handler[[X] =>> Result, Elim, Any]

  /** Absorbs effects requested to create the handler, as the new handler's additional dependencies. */
  def flatHandle[F[+_], L, N1, N2](h: Handler[F, L, N1] !! N2): Handler[F, L, N1 & N2] = HandlerCases.FlatHandled(h)


private[turbolift] object HandlerCases:
  final case class Primitive[Result[+_], Elim, Intro](
    interpreter: Interpreter.Apply[Result, Elim, Intro],
    initial: Any,
  ) extends Handler[Result, Elim, Intro]:
    override def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro) = 
      new ComputationCases.Delimit[A, U, Result, Elim, Intro](comp, this)


  final case class Composed[Result1[+_], Result2[+_], Elim1, Elim2, Intro1, Intro2, Hidden](
    first: Handler[Result1, Elim1, Intro1 & Hidden],
    second: Handler[Result2, Elim2 & Hidden, Intro2],
  ) extends Handler[[X] =>> Result2[Result1[X]], Elim1 & Elim2, Intro1 & Intro2]:
    override def doHandle[A, U](comp: A !! (U & Elim1 & Elim2)): Result2[Result1[A]] !! (U & Intro1 & Intro2) =
      second.doHandle[Result1[A], U & Intro1](
        first.doHandle[A, U & Elim2](comp)
      )
  

  final case class Mapped[OldResult[+_], NewResult[+_], Elim, Intro](
    that: Handler[OldResult, Elim, Intro],
    fun: [X] => OldResult[X] => NewResult[X],
  ) extends Handler[NewResult, Elim, Intro]:
    override def doHandle[A, U](comp: A !! (U & Elim)): NewResult[A] !! (U & Intro) =
      that.doHandle[A, U](comp).map(fun(_))


  final case class FlatMapped[OldResult[+_], NewResult[+_], Elim, Intro1, Intro2](
    that: Handler[OldResult, Elim, Intro1],
    fun: [X] => OldResult[X] => NewResult[X] !! Intro2,
  ) extends Handler[NewResult, Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: A !! (U & Elim)): NewResult[A] !! (U & Intro1 & Intro2) =
      that.doHandle[A, U](comp).flatMap(fun(_))


  final case class FlatTapped[Result[+_], Elim, Intro1, Intro2](
    that: Handler[Result, Elim, Intro1],
    fun: [X] => Result[X] => Unit !! Intro2,
  ) extends Handler[Result, Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro1 & Intro2) =
      that.doHandle[A, U](comp).flatTap(fun(_))


  final case class FlatHandled[Result[+_], Elim, Intro1, Intro2](
    that: Handler[Result, Elim, Intro1] !! Intro2,
  ) extends Handler[Result[+_], Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro1 & Intro2) =
      that.flatMap(_.doHandle[A, U](comp))

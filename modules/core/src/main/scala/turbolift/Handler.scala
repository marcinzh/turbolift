package turbolift
import turbolift.internals.auxx.CanPartiallyHandle
import turbolift.internals.extensions.HandlerExtensions
import turbolift.internals.interpreter.Interpreter
import turbolift.internals.primitives.{ComputationCases => CC}

/** Used to delimit scope of effect(s).
 *
 *  Handler transforms a [[Computation]], by discharging some or all of its requested effects,
 *  and (optionally) introducing handlers own dependencies to it (set of effects used to create the handler).
 *  {{{
 *  val myComputation2 = myComputation1.handleWith(myHandler)
 *  }}}
 *  
 *  Handlers can be obtained by:
 *  - implementing an [[internals.interpreter.Interpreter Interpreter]] for an [[Effect]],
 *  and then calling `toHandler` method on it.
 *  - transforming a preexisting handler, e.g. `val myHandler2 = myHandler1.map(...)`
 *  - composing 2 preexisting handlers, e.g. `val myHandler3 = myHandler1 &&&! myHandler2`
 *  
 *  Compositon of 2 handlers is always *sequential*: the operands are applied in left to right order.
 *  
 *  @tparam Result Type constructor (e.g. `Option[_]`), in which the computation's result is wrapped, after application of this handler. This is often an identity.
 *  @tparam Elim Type-level set of effects, that this handler __eliminates__ from the computation.
 *  @tparam Intro Type-level set of effects, that this handler __introduces__ into the computation. This is often an empty set, expressed as `Any`.
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

  /** Transforms this handler, by applying a post-processing function to its result`.
    *
    * a.k.a Natural Transformation.
    */
  final def map[NewResult[+_]](f: [X] => Result[X] => NewResult[X]): Handler[NewResult, Elim, Intro] =
    HandlerCases.Mapped[Result, NewResult, Elim, Intro](this, f)

  /** Like [[map]], but the post-processing of `Result[_]` can also introduce effects.
    *
    * Those effects are then absorbed by the new handler into the effects it introduces.
    */
  final def flatMap[NewResult[+_], V](f: [X] => Result[X] => NewResult[X] !! V): Handler[NewResult, Elim, Intro & V] =
    HandlerCases.FlatMapped[Result, NewResult, Elim, Intro, V](this, f)

  /** Like [[flatMap]], but the post-processing is executed for its effects only.
    *
    * This handler's `Result[_]` remains unchanged.
    */
  final def flatTap[V](f: [X] => Result[X] => Unit !! V): Handler[Result, Elim, Intro & V] =
    flatMap([X] => (xx: Result[X]) => f(xx).as(xx))

  /** Composes 2 **independent** handlers.
    *
    * Independence of handlers means, that effects __eliminated__ by one of the handlers,
    * do not overlap with effects __introduced__ by the other.
    *
    * Independence of 2 handlers guarantees, that it's also valid to compose them in the opposite order.
    * However, nesting order of their `Result[_]`s would also be reversed.
    */
  final def composeWith[ThatResult[+_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, ThatElim, Intro, ThatIntro, Any](this, that).self
  
  /** Composes 2 **fully dependent** handlers.
    *
    * Assumes that **all** effects introduced by this handler, are eliminated by `that` handler.
    */
  final def provideWith[ThatResult[+_], ThatIntro](that: Handler[ThatResult, Intro, ThatIntro]) =
    HandlerCases.Composed[Result, ThatResult, Elim, Any, Any, ThatIntro, Intro](Handler.this, that).self

  /** Composes 2 **partially dependent** handlers.
    *
    * Assumes that **some of** effects introduced by this handler, are eliminated by `that` handler.
    */
  final def partiallyProvideWith[Remains >: Intro] = new PartiallyProvideWithApply[Remains]
  class PartiallyProvideWithApply[Remains >: Intro]:
    def apply[ThatResult[+_], ThatElim >: Intro, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) =
     HandlerCases.Composed[Result, ThatResult, Elim, Any, Remains, ThatIntro, ThatElim](upCastIntro[Remains & ThatElim], that).self

  /** Alias for [[provideWith]]. */
  final def %%%![ThatResult[+_], ThatIntro](that: Handler[ThatResult, Intro, ThatIntro]) = provideWith(that)

  /** Alias for [[partiallyProvideWith]]. */
  final def %%![Remains >: Intro] = partiallyProvideWith[Remains]

  private[turbolift] final def upCastIntro[T >: Intro] = asInstanceOf[Handler[Result, Elim, T]]

  private[turbolift] final def self: Handler[Result, Elim, Intro] = this

  /** Alias for [[composeWith]]. */
  final def &&&![ThatResult[+_], ThatElim, ThatIntro](that: Handler[ThatResult, ThatElim, ThatIntro]) = this.composeWith(that)

  /** Transforms this handler, by discarding its result. */
  final def void: Handler.Const[Unit, Elim, Intro] = map([X] => (_: Result[X]) => ())


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

  /** Transforms a computation of a handler, into a new handler.
   *
   *  Useful for effectful creation of handlers.
   *  Effects requested to create the handler are absorbed by the handler itself,
   *  into its own (additional) dependencies.
   */
  def flatHandle[F[+_], L, N1, N2](h: Handler[F, L, N1] !! N2): Handler[F, L, N1 & N2] = HandlerCases.FlatHandled(h)


private[turbolift] object HandlerCases:
  final case class Primitive[Result[+_], Elim, Intro](
    interpreter: Interpreter.Apply[Result[+_], Elim, Intro],
    initial: Any
  ) extends Handler[Result, Elim, Intro]:
    override def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro) = 
      new CC.Handle[A, U, Result, Elim, Intro](comp, this)


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


  final case class FlatHandled[Result[+_], Elim, Intro1, Intro2](
    that: Handler[Result, Elim, Intro1] !! Intro2,
  ) extends Handler[Result[+_], Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: A !! (U & Elim)): Result[A] !! (U & Intro1 & Intro2) =
      that.flatMap(_.doHandle[A, U](comp))

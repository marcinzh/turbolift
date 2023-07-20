package turbolift
import turbolift.internals.auxx.{CanPartiallyHandle, CanPipe}
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
  final def handle[V] = new HandleApply[V]
  final class HandleApply[V]:
    def apply[A, W](comp: From[A] !! W)(implicit ev: CanPartiallyHandle[V, W, Elim]): To[A] !! (V & Intro) =
      doHandle[A, V](ev(comp))

  /** Transforms this handler, by applying a post-processing function to its result`.
    *
    * a.k.a Natural Transformation.
    */
  final def map[To2[+_]](f: [X] => To[X] => To2[X]): Handler[From, To2, Elim, Intro] =
    flatMap([X] => (xx: To[X]) => !!.pure(f(xx)))
    // HandlerCases.Mapped[From, To, To2, Elim, Intro](this, f)

  /** Like [[map]], but the post-processing of `To[_]` can also introduce effects.
    *
    * Those effects are then absorbed by the new handler into the effects it introduces.
    */
  final def flatMap[To2[+_], V](f: [X] => To[X] => To2[X] !! V): Handler[From, To2, Elim, Intro & V] =
    HandlerCases.FlatMapped[From, To, To2, Elim, Intro, V](this, f)

  /** Like [[flatMap]], but the post-processing is executed for its effects only.
    *
    * This handler's `To[_]` remains unchanged.
    */
  final def flatTap[V](f: [X] => To[X] => Unit !! V): Handler[From, To, Elim, Intro & V] =
    flatMap([X] => (xx: To[X]) => f(xx).as(xx))

  /** Transforms this handler, by applying a pre-processing function to its input`. */
  final def contraMap[From2[+_]](f: [X] => From2[X] => From[X]): Handler[From2, To, Elim, Intro] =
    contraFlatMap([X] => (xx: From2[X]) => !!.pure(f(xx)))

  /** Like [[contraMap]], but the pre-processing of `From[_]` can also introduce effects.
    *
    * Those effects are then absorbed by the new handler into the effects it introduces.
    */
  final def contraFlatMap[From2[+_], V](f: [X] => From2[X] => From[X] !! V): Handler[From2, To, Elim, Intro & V] =
    HandlerCases.ContraFlatMapped[From2, From, To, Elim, V, Intro](f, this)

  /** Like [[contraFlatMap]], but the pre-processing is executed for its effects only.
    *
    * This handler's `From[_]` remains unchanged.
    */
  final def contraFlatTap[V](f: [X] => From[X] => Unit !! V): Handler[From, To, Elim, Intro & V] =
    contraFlatMap([X] => (xx: From[X]) => f(xx).as(xx))

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
  final def partiallyProvideWith[Remains >: Intro] = new PartiallyProvideWithApply[Remains]
  class PartiallyProvideWithApply[Remains >: Intro]:
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
  final def void: Handler[From, [X] =>> Unit, Elim, Intro] = map([X] => (_: To[X]) => ())


object Handler extends HandlerExtensions:
  /** Alias for handler, whose `From[_]` is type-level identity. */
  type FromId[To[+_], Elim, Intro] = Handler[[X] =>> X, To, Elim, Intro]

  /** Alias for handler, whose both `From[_]` and `To[_]` are type-level identities. */
  type Id[Elim, Intro] = FromId[[X] =>> X, Elim, Intro]

  /** Alias for handler, whose `From[_]` is type-level identity, and `To[_]` is a type-level constant function. */
  type Const[To, Elim, Intro] = FromId[[_] =>> To, Elim, Intro]

  /** Alias for handler, whose `From[_]` is type-level identity, and has no dependencies (introduces no new effects). */
  type Free[To[+_], Elim] = FromId[To, Elim, Any]

  object Free:
    /** Alias for handler, that is both [[Free]] and [[Id]]. */
    type Id[Elim] = Free[[X] =>> X, Elim]

    /** Alias for handler, that is both [[Free]] and [[Const]]. */
    type Const[To, Elim] = Free[[_] =>> To, Elim]
 
  /** Transforms a computation of a handler, into a new handler.
   *
   *  Useful for effectful creation of handlers.
   *  Effects requested to create the handler are absorbed by the handler itself,
   *  into its own (additional) dependencies.
   */
  def flatHandle[F[+_], G[+_], L, N1, N2](h: Handler[F, G, L, N1] !! N2): Handler[F, G, L, N1 & N2] = HandlerCases.FlatHandled(h)

  def identity[F[+_]]: Handler[F, F, Any, Any] = HandlerCases.Identity[F]()


private[turbolift] object HandlerCases:
  final case class Identity[F[+_]]() extends Handler[F, F, Any, Any]:
    override def doHandle[A, U](comp: F[A] !! U): F[A] !! U = comp


  final case class Primitive[From[+_], To[+_], Elim, Intro](
    interpreter: Interpreter.Apply[From, To, Elim, Intro],
    initial: Any
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
  

  final case class FlatMapped[From[+_], To1[+_], To2[+_], Elim, Intro1, Intro2](
    that: Handler[From, To1, Elim, Intro1],
    fun: [X] => To1[X] => To2[X] !! Intro2,
  ) extends Handler[From, To2, Elim, Intro1 & Intro2]:
    override def doHandle[A, U](comp: From[A] !! (U & Elim)): To2[A] !! (U & Intro1 & Intro2) =
      that.doHandle[A, U](comp).flatMap(fun(_))


  final case class ContraFlatMapped[From1[+_], From2[+_], To[+_], Elim, Intro1, Intro2](
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

package turbolift
import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration
import turbolift.effects.{ChoiceSignature, IO, Each, Finalizer}
import turbolift.internals.auxx.CanPartiallyHandle
import turbolift.internals.effect.AnyChoice
import turbolift.internals.executor.Executor
import turbolift.interpreter.Prompt
import turbolift.internals.engine.{Env, Engine, Tag}
import turbolift.io.{Outcome, Fiber, Warp}
import turbolift.mode.Mode
import turbolift.{ComputationCases => CC}


/** Alias for [[Computation]] type. Meant to be used in infix form. */
type !![+A, -U] = Computation[A, U]

/** Alias for [[Computation]] companion object. */
def !! = Computation


/** Monad parametrized by a set of requested effect. Use the `!!` infix type alias instead.
 *   
 * Type-level set of effects is modelled with intersection types. 
 * Type `Any` means empty set.
 *
 * {{{
 * type MyComputationType1 = String !! (MyState & MyError)
 * type MyComputationType2 = String !! Any
 * }}}
 *
 * @tparam A Result of the computation
 * @tparam U Type-level set of effects requested by this computation.
 */

sealed abstract class Computation[+A, -U] private[turbolift] (private[turbolift] val tag: Tag):
  private final def map_bug[B](f: A => B): B !! U = map(f)

  @nowarn("msg=anonymous class definition")
  final inline def flatMap[B, U2 <: U](inline f: A => B !! U2): B !! U2 =
    new CC.FlatMap[A, B, U2](this):
      override def apply(a: A): B !! U2 = f(a)

  @nowarn("msg=anonymous class definition")
  final inline def map[B](inline f: A => B): B !! U =
    new CC.PureMap[A, B, U](this):
      override def apply(a: A): B = f(a)

  final def flatten[B, U2 <: U](implicit ev: A <:< (B !! U2)): B !! U2 = flatMap(ev)
  @deprecated final def flatTap[B, U2 <: U](f: A => B !! U2): A !! U2 = tapEff(f)
  final def tapEff[B, U2 <: U](f: A => B !! U2): A !! U2 = flatMap(a => f(a).as(a))

  /** Composes 2 independent computations sequentially */
  final def zip[B, U2 <: U](that: => B !! U2): (A, B) !! U2 = flatMap(a => that.map_bug((a, _)))

  /** Composes 2 independent computations parallelly (if possible).
   *
   * Parallelism may be impossible, due to at least one of handlers in current scope
   * being inherently sequential (e.g. `State.handlers.local` or `Error.handlers.first`).
   * In such case, `zipPar` behaves like `zip`.
   */
  final def zipPar[B, U2 <: U](that: B !! U2): (A, B) !! U2 = zipWithPar(that)(Computation.pairCtorFun[A, B])

  /** Like [[zip]], but followed by untupled `map`. */
  final def zipWith[B, C, U2 <: U](that: => B !! U2)(f: (A, B) => C): C !! U2 = flatMap(a => that.map(f(a, _)))

  /** Like [[zipPar]], but followed by untupled `map`. */
  final def zipWithPar[B, C, U2 <: U](that: B !! U2)(f: (A, B) => C): C !! U2 = CC.intrinsic(_.intrinsicZipPar(this, that, f))
  
  /** Discards the result, and replaces it by given pure value. */
  final def as[B](value: B): B !! U = map(_ => value)

  /** Discards the result, and replaces it by `Unit`. */
  final def void: Unit !! U = as(())

  /** Alias for [[flatMap]]. */
  final def >>=[B, U2 <: U](f: A => B !! U2): B !! U2 = flatMap(f)

  /** Alias for [[zipPar]]. */
  final def *![B, U2 <: U](that: B !! U2): (A, B) !! U2 = zipPar(that)

  /** Alias for [[zip]]. */
  final def **![B, U2 <: U](that: => B !! U2): (A, B) !! U2 = zip(that)

  /** Composes 2 independent computations parallelly (if possible), discarding result of the first.
   *
   * Parallelism may be impossible, due to at least one of handlers in current scope
   * being inherently sequential (e.g. `State.handlers.local` or `Error.handlers.first`).
   * In such case, `&!` behaves like `&&!`.
   */
  final def &![B, U2 <: U](that: B !! U2): B !! U2 = zipPar(that).map_bug(_._2)
  
  /** Composes 2 independent computations sequentially, discarding result of the first. */
  final def &&![B, U2 <: U](that: => B !! U2): B !! U2 = flatMap(_ => that)

  /** Composes 2 independent computations parallelly (if possible), discarding result of the second.
   *
   * Parallelism may be impossible, due to at least one of handlers in current scope
   * being inherently sequential (e.g. `State.handlers.local` or `Error.handlers.first`).
   * In such case, `&<!` behaves like `&&<!`.
   */
  final def &<![B, U2 <: U](that: B !! U2): A !! U2 = zipPar(that).map_bug(_._1)

  /** Composes 2 independent computations sequentially, discarding result of the second. */
  final def &&<![B, U2 <: U](that: => B !! U2): A !! U2 = flatMap(a => that.as(a))

  /** Races 2 computations.
   *
   * Runs both computations parallelly, each in fresh fiber.
   * Once one of them finishes, the other is cancelled.
   */
  final def |![A2 >: A, U2 <: U & IO](that: A2 !! U2): A2 !! U2 = CC.intrinsic(_.intrinsicOrPar(this, that))

  /** Sequential "or-else" operator.
   *
   * Runs the first computations in fresh fiber.
   * If it ends up cancelled, the second computation is run.
   */
  final def ||![A2 >: A, U2 <: U & IO](that: => A2 !! U2): A2 !! U2 = CC.intrinsic(_.intrinsicOrSeq(this, () => that))

  /** Parallel version of `++!`. */
  final def +![A2 >: A, U2 <: U & ChoiceSignature](that: A2 !! U2): A2 !! U2 = AnyChoice.plusPar(this, that)

  /** Applies `plus` operation from the innermost `Choice` effect in the current scope.
   *
   * Similar to `<|>` operator of `Alternative`.
   */
  final def ++![A2 >: A, U2 <: U & ChoiceSignature](that: => A2 !! U2): A2 !! U2 = AnyChoice.plus(this, that)

  /** Applies filter, using `empty` operation from the innermost `Choice` effect in the current scope. */
  final def withFilter[U2 <: U & ChoiceSignature](f: A => Boolean): A !! U2 = flatMap(a => if f(a) then !!.pure(a) else !!.empty)

  /** Widens the set of requested effects. */
  final def upCast[U2 <: U] = this: A !! U2

  final def cast[A2, U2]: A2 !! U2 = asInstanceOf[Computation[A2, U2]]

  private[turbolift] final def untyped = this.asInstanceOf[Any !! Any]

  //@#@OLD
  // final override def toString = s"turbolift.Computation@${hashCode.toHexString}"

  //---------- IO operations in postfix syntax ----------

  /** Run this computation in a new fiber. */
  final def fork: Fiber[A, U] !! (U & IO & Warp) = Fiber.fork(this)

  /** Like [[fork]], but the fiber is created as a child of specific warp, rather than the current warp. */
  final def forkAt(warp: Warp): Fiber[A, U] !! (U & IO) = Fiber.forkAt(warp)(this)

  final def onFailure[U2 <: U & IO](f: Throwable => Unit !! U2): A !! U2 = IO.onFailure(this)(f)

  final def onCancel[U2 <: U & IO](comp: Unit !! U2): A !! U2 = IO.onCancel(this)(comp)

  final def guarantee[U2 <: U & IO](release: Unit !! U2): A !! U2 = IO.guarantee(release)(this)

  final def executeOn[U2 <: U & IO](exec: Executor): A !! U2 = IO.executeOn(exec)(this)

  final def delay[U2 <: U & IO](millis: Long): A !! U2 = IO.delay(millis)(this)

  final def delay[U2 <: U & IO](duration: FiniteDuration): A !! U2 = IO.delay(duration)(this)

  /** Syntax for giving names to fibers. */
  final def named[A2 >: A, U2 <: U](name: String) = new Computation.NamedSyntax[A2, U2](this, name)


/**
  * Use the `!!` alias to access methods of this companion object.
  * 
  * Example:
  * {{{
  * import turbolift.!!
  * 
  * val myComputation: Int !! Any = !!.pure(42)
  * }}}
  */


object Computation:
  private[turbolift] type Untyped = Computation[Any, Any]

  private def pairCtorFun[A, B]: (A, B) => (A, B) = pairCtorVal.asInstanceOf[(A, B) => (A, B)]
  private val pairCtorVal: (Any, Any) => Any = (_, _) 

  /** Same as `!!.pure(())`. */
  val unit: Unit !! Any = pure(())

  /** Same as `!!.pure(None)`. */
  val none: Option[Nothing] !! Any = pure(None)

  /** Same as `!!.pure(Nil)`. */
  val nil: List[Nothing] !! Any = pure(Nil)

  /** Same as `!!.pure(Vector())`. */
  val vector: Vector[Nothing] !! Any = pure(Vector())

  def pure[A](a: A): A !! Any = new CC.Pure(a)

  @nowarn("msg=anonymous class definition")
  inline def impure[A](inline a: => A): A !! Any =
    new CC.Impure[A]:
      override def apply(): A = a

  def impureEff[A, U](comp: => A !! U): A !! U = unit.flatMap(_ => comp)
  @deprecated def defer[A, U](comp: => A !! U): A !! U = impureEff(comp)

  /** Executes `empty` operation from the innermost `Choice` effect in the current scope. */
  def empty: Nothing !! ChoiceSignature = AnyChoice.empty

  /** Handles `Each` effect. */
  def every[A, U](body: A !! (U & Each)): Vector[A] !! U = body.handleWith(Each.handler)

  /** Handles `Each` effect, discarding the result. */
  def everyVoid[A, U](body: Unit !! (U & Each)): Unit !! U = body.handleWith(Each.void)

  /** Like `if`-`then` statement, but the body is a computation */
  def when[U](cond: Boolean)(body: => Unit !! U): Unit !! U = if cond then body else unit

  /** Repeats the computation, given number of times. */
  def repeat[U](n: Int)(body: => Unit !! U): Unit !! U =
    def loop(n: Int): Unit !! U =
      if n > 0 then
        body &&! loop(n - 1)
      else
        unit
    loop(n)

  /** Repeats the computation, while the effectful condition is true. */
  def repeatWhile[U, U2 <: U](cond: Boolean !! U)(body: => Unit !! U2): Unit !! U2 =
    def loop: Unit !! U2 = cond.flatMap(if _ then body &&! loop else unit)
    loop

  /** Repeats the computation, while the effectful condition is false. */
  def repeatUntil[U, U2 <: U](cond: Boolean !! U)(body: => Unit !! U2): Unit !! U2 =
    def loop: Unit !! U2 = cond.flatMap(if _ then unit else body &&! loop)
    loop

  /** Like [[repeat]], but returns results of each iteration. */
  def replicate[A, U](n: Int)(body: => A !! U): Vector[A] !! U =
    def loop(n: Int, acc: Vector[A]): Vector[A] !! U =
      if n > 0 then
        body.flatMap(a => loop(n - 1, acc :+ a))
      else
        pure(acc)
    loop(n, Vector())

  /** Like [[iterate]], but returns results of each iteration. */
  def generate[A, B, U](init: A, cond: A => Boolean, inc: A => A)(body: A => B !! U): Vector[B] !! U =
    def loop(a: A, acc: Vector[B]): Vector[B] !! U =
      if cond(a) then
        body(a).flatMap(b => loop(inc(a), acc :+ b))
      else
        pure(acc)
    loop(init, Vector())

  /** Like the standard `for` loop, but using computation instead of statement.
   *
   *  Returns last value of the iterated variable.
   */
  def iterate[A, U](init: A, cond: A => Boolean, inc: A => A)(body: A => Unit !! U): A !! U =
    def loop(a: A): A !! U =
      if cond(a) then
        body(a) &&! loop(inc(a))
      else
        pure(a)
    loop(init)

  /** Like the standard `for` loop, but using computation instead of statement.
   *
   *  Returns last value of the iterated variable.
   */
  def iterateWhile[A, U](init: A, cond: A => Boolean)(body: A => A !! U): A !! U =
    def loop(a: A): A !! U =
      if cond(a) then
        body(a) >>= loop
      else
        pure(a)
    loop(init)

  /** Like the standard `for` loop, but using computation instead of statement.
   *
   *  Returns last value of the iterated variable.
   */
  def iterateUntil[A, U](init: A, cond: A => Boolean)(body: A => A !! U): A !! U =
    iterateWhile(init, a => !cond(a))(body)

  def envAsk[A](f: Env => A): A !! Any = CC.intrinsic(_.intrinsicEnvAsk(f))
  def envMod[A, U](f: Env => Env, body: A !! U): A !! U = CC.intrinsic(_.intrinsicEnvMod(f, body))

  def isParallel: Boolean !! Any = envAsk(_.isParallelismRequested)
  def isSequential: Boolean !! Any = isParallel.map(!_)
  def parallellyIf[A, U](cond: Boolean)(body: A !! U): A !! U = envMod(_.par(cond), body)
  def sequentiallyIf[A, U](cond: Boolean)(body: A !! U): A !! U = parallellyIf(!cond)(body)
  def parallelly[A, U](body: A !! U): A !! U = parallellyIf(true)(body)
  def sequentially[A, U](body: A !! U): A !! U = parallellyIf(false)(body)


  //---------- Extensions ----------


  extension [A](thiz: Computation[A, Any])
    /** Runs the computation, provided that it requests no effects. */
    def run(using mode: Mode = Mode.default): A = Executor.pick(mode).runSync(thiz, "").get

    def runST: A = Executor.ST.runSync(thiz, "").get
    def runMT: A = Executor.MT.runSync(thiz, "").get


  extension [A](thiz: Computation[A, IO])
    /** Runs the computation, provided that it requests IO effect only, or none at all. */
    def runIO(using mode: Mode = Mode.default): Outcome[A] = Executor.pick(mode).runSync(thiz, "")

    def runAsync(using mode: Mode = Mode.default)(callback: Outcome[A] => Unit): Unit = Executor.pick(mode).runAsync(thiz, "", callback)

    def runIOST: Outcome[A] = Executor.ST.runSync(thiz, "")
    def runIOMT: Outcome[A] = Executor.MT.runSync(thiz, "")

    /*@deprecated*/ def unsafeRun(using mode: Mode = Mode.default): Outcome[A] = runIO
    /*@deprecated*/ def unsafeRunAsync(using mode: Mode = Mode.default)(callback: Outcome[A] => Unit): Unit = runAsync(callback)
    /*@deprecated*/ def unsafeRunST: Outcome[A] = Executor.ST.runSync(thiz, "")
    /*@deprecated*/ def unsafeRunMT: Outcome[A] = Executor.MT.runSync(thiz, "")


  extension [A, U <: IO](thiz: Computation[A, U & Warp])
    def warp: A !! U = thiz.warpCancel
    def warp(exitMode: Warp.ExitMode, name: String = ""): A !! U = thiz.handleWith(Warp.handler(exitMode, name))
    def warpCancel: A !! U = warpCancel("")
    def warpAwait: A !! U = warpAwait("")
    def warpCancel(name: String): A !! U = warp(Warp.ExitMode.Cancel, name)
    def warpAwait(name: String): A !! U = warp(Warp.ExitMode.Await, name)


  extension [A, U <: IO](thiz: Computation[A, U & Finalizer])
    def finalized: A !! U = Finalizer.scoped(thiz)


  extension [A, U](thiz: Computation[A, U])
    def downCast[U2 >: U] = thiz.asInstanceOf[Computation[A, U2]]
    def castEv[U2](using ev: U2 =:= U) = thiz.asInstanceOf[Computation[A, U2]]
  
    /** Creates a handler effectfully.
     * 
     *  Passes computed value to handler constructor.
     *  Effect used to compute the value are absorbed by the handler, into its own dependencies.
     */ 
    def flatMapHandler[F[+_], G[+_], L, N](f: A => Handler[F, G, L, N]): Handler[F, G, L, U & N] = Handler.flatHandle(thiz.map(f))

    /** Alias of [[flatMapHandler]] */
    def >>=![F[+_], G[+_], L, N](f: A => Handler[F, G, L, N]): Handler[F, G, L, U & N] = Handler.flatHandle(thiz.map(f))

    /** Applies a handler to this computation.
     *
     *  Same as `myHandler.handle(this)`.
     */
    def handleWith[V]: HandleWithSyntax[A, U, V] = new HandleWithSyntax[A, U, V](thiz)


  extension [F[+_], G[+_], L, N](thiz: Computation[Handler[F, G, L, N], N])
    /** Simplifies effectful creation of handlers.
     * 
     *  Same as `computation.flatMapHandler(handler => handler)`.
     */
    def flattenHandler: Handler[F, G, L, N] = Handler.flatHandle(thiz)


  extension [A, B, U](thiz: Computation[(A, B), U])
    def map2[C](f: (A, B) => C): C !! U = thiz.map(f.tupled)
    def flatMap2[C, U2 <: U](f: (A, B) => C !! U2): C !! U2 = thiz.flatMap(f.tupled)


  extension [U](thiz: Computation[Boolean, U])
    /** Like `while` statement, but the condition and the body are computations. */
    def whileEff[U2 <: U](body: => Unit !! U2): Unit !! U2 = !!.repeatWhile(thiz)(body)

    /** Like `while` statement, but the condition and the body are computations. */
    def untilEff[U2 <: U](body: => Unit !! U2): Unit !! U2 = !!.repeatUntil(thiz)(body)

    /** Like `if` statement, but the condition and the body are computations. */
    def ifEff[U2 <: U](thenBody: => Unit !! U2)(elseBody: => Unit !! U2): Unit !! U2 =
      thiz.flatMap(if _ then thenBody else elseBody)


  //---------- Syntax ----------

  final class HandleWithSyntax[A, U, V](thiz: A !! U):
    def apply[F[+_], G[+_], L, N](h: Handler[F, G, L, N])(using ev1: A =:= F[A], ev2: (L & V) <:< U): G[A] !! (N & V) =
      h.doHandle(thiz.cast[F[A], L & V])

  final class NamedSyntax[A, U](val comp: Computation[A, U], val name: String):
    def fork: Fiber[A, U] !! (U & IO & Warp) = Fiber.named(name).fork(comp)
    def forkAt(warp: Warp): Fiber[A, U] !! (U & IO) = Fiber.named(name).forkAt(warp)(comp)
    def run(using mode: Mode = Mode.default, ev: Any <:< U): Outcome[A] = Executor.pick(mode).runSync(comp, name)

  object NamedSyntax:
    extension [A](thiz: NamedSyntax[A, IO])
      def runIO(using mode: Mode = Mode.default): Outcome[A] = Executor.pick(mode).runSync(thiz.comp, thiz.name)
      def runAsync(using mode: Mode = Mode.default)(callback: Outcome[A] => Unit): Unit = Executor.pick(mode).runAsync(thiz.comp,thiz. name, callback)


//@#@TEMP public bcoz inline bug
// private[turbolift] object ComputationCases:
object ComputationCases:
  private[turbolift] final class Pure[A](val value: A) extends Computation[A, Any](Tag.Pure)
  private[turbolift] final class LocalGet(val prompt: Prompt) extends Computation[Any, Any](Tag.LocalGet)
  private[turbolift] final class LocalPut[S](val prompt: Prompt, val local: S) extends Computation[Unit, Any](Tag.LocalPut)
  //@#@TEMP public bcoz inline bug
  sealed abstract class FlatMap[A, B, U](val comp: A !! U) extends Computation[B, U](Tag.FlatMap) with Function1[A, B !! U]
  sealed abstract class PureMap[A, B, U](val comp: A !! U) extends Computation[B, U](Tag.PureMap) with Function1[A, B]
  sealed abstract class Impure[A]() extends Computation[A, Any](Tag.Impure) with Function0[A]
  sealed abstract class Sync[A, B](val isAttempt: Boolean) extends Computation[B, IO](Tag.Sync) with Function0[A]
  sealed abstract class Intrinsic[A, U] extends Computation[A, U](Tag.Intrinsic) with Function[Engine, Tag]
  sealed abstract class Perform[A, U, Z <: Signature](val sig: Signature) extends Computation[A, U](Tag.Perform) with Function1[Z, A !! U]
  sealed abstract class LocalUpdate[A, S](val prompt: Prompt) extends Computation[A, Any](Tag.LocalUpdate) with Function1[S, (A, S)]

  @nowarn("msg=anonymous class definition")
  private[turbolift] inline def sync[A, B](isAttempt: Boolean, inline thunk: => A): B !! IO =
    new Sync[A, B](isAttempt):
      override def apply(): A = thunk

  @nowarn("msg=anonymous class definition")
  private[turbolift] inline def intrinsic[A, U](f: Engine => Tag): A !! U =
    new CC.Intrinsic[A, U]:
      override def apply(engine: Engine): Tag = f(engine)

  @nowarn("msg=anonymous class definition")
  private[turbolift] inline def perform[A, U, Z <: Signature](sig: Signature, inline f: Z => A !! U): A !! U =
    new Perform[A, U, Z](sig):
      override def apply(z: Z): A !! U = f(z)

  @nowarn("msg=anonymous class definition")
  private[turbolift] inline def localUpdate[A, S](prompt: Prompt, inline f: S => (A, S)): A !! Any =
    new LocalUpdate[A, S](prompt):
      override def apply(s: S): (A, S) = f(s)

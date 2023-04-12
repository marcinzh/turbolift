package turbolift
import turbolift.effects.{ChoiceSignature, Each}
import turbolift.internals.effect.AnyChoice
import turbolift.internals.extensions.ComputationExtensions
import turbolift.internals.primitives.Primitives


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

sealed abstract class Computation[+A, -U] private[turbolift] (private[turbolift] val tag: Int):
  final def map[B](f: A => B): B !! U = Primitives.map(this, f)
  final def flatMap[B, U2 <: U](f: A => B !! U2): B !! U2 = Primitives.flatMap(this, f)
  final def flatten[B, U2 <: U](implicit ev: A <:< (B !! U2)): B !! U2 = flatMap(ev)
  final def flatTap[B, U2 <: U](f: A => B !! U2): A !! U2 = flatMap(a => f(a).as(a))

  /** Composes 2 independent computations sequentially */
  final def zip[B, U2 <: U](that: => B !! U2): (A, B) !! U2 = flatMap(a => that.map((a, _)))

  /** Composes 2 independent computations parallelly (if possible).
   *
   * Parallelism may be impossible, due to at least one of handlers in current scope
   * being inherently sequential (e.g. `State.handlers.local` or `Error.handlers.first`).
   * In such case, `zipPar` behaves like `zip`.
   */
  final def zipPar[B, U2 <: U](that: B !! U2): (A, B) !! U2 = Primitives.zipPar(this, that)

  /** Like [[zip]], but followed by untupled `map`. */
  final def zipWith[B, C, U2 <: U](that: => B !! U2)(f: (A, B) => C): C !! U2 = flatMap(a => that.map(f(a, _)))

  /** Like [[zipPar]], but followed by untupled `map`. */
  final def zipWithPar[B, C, U2 <: U](that: B !! U2)(f: (A, B) => C): C !! U2 = Primitives.zipWithPar(this, that, f)
  
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
  final def &![B, U2 <: U](that: B !! U2): B !! U2 = zipPar(that).map(_._2)
  
  /** Composes 2 independent computations sequentially, discarding result of the first. */
  final def &&![B, U2 <: U](that: => B !! U2): B !! U2 = flatMap(_ => that)

  /** Composes 2 independent computations parallelly (if possible), discarding result of the second.
   *
   * Parallelism may be impossible, due to at least one of handlers in current scope
   * being inherently sequential (e.g. `State.handlers.local` or `Error.handlers.first`).
   * In such case, `&<!` behaves like `&&<!`.
   */
  final def &<![B, U2 <: U](that: B !! U2): A !! U2 = zipPar(that).map(_._1)

  /** Composes 2 independent computations sequentially, discarding result of the second. */
  final def &&<![B, U2 <: U](that: => B !! U2): A !! U2 = flatMap(a => that.map(_ => a))

  /** Applies `plus` operation from the innermost `Choice` effect in the current scope.
   *
   * Similar to `<|>` operator of `Alternative`.
   */
  final def ++![A2 >: A, U2 <: U & ChoiceSignature](that: => A2 !! U2): A2 !! U2 = AnyChoice.plus(this, that)

  /** Applies filter, using `fail` operation from the innermost `Choice` effect in the current scope. */
  final def withFilter[U2 <: U & ChoiceSignature](f: A => Boolean): A !! U2 = flatMap(a => if f(a) then !!.pure(a) else !!.fail)

  /** Widens the set of requested effects. */
  final def upCast[U2 <: U] = this: A !! U2

  private[turbolift] final def untyped = this.asInstanceOf[Any !! Any]

  final override def toString = s"turbolift.Computation@${hashCode.toHexString}"

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

object Computation extends ComputationExtensions:
  private[turbolift] abstract class Unsealed[A, U](_tag: Int) extends Computation[A, U](_tag)
  private[turbolift] type Untyped = Computation[Any, Any]

  /** Same as `!!.pure(())`. */
  val unit = pure(())

  def pure[A](a: A): A !! Any = Primitives.pure(a)
  def defer[A, U](ua: => A !! U): A !! U = unit.flatMap(_ => ua)
  
  def impure[A](a: => A): A !! Any = Primitives.impure(() => a)

  /** Executes `fail` operation from the innermost `Choice` effect in the current scope. */
  def fail: Nothing !! ChoiceSignature = AnyChoice.fail

  /** Handles `Each` effect. */
  def every[A, U](body: A !! (U & Each)): Vector[A] !! U = body.handleWith(Each.handler)

  /** Handles `Each` effect, discarding the result. */
  def everyVoid[A, U](body: Unit !! (U & Each)): Unit !! U = body.handleWith(Each.void)

  /** Executes the computation, if the condition is true. */
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

  def isParallel: Boolean !! Any = Primitives.configAsk(_.isParallelismRequested)
  def isSequential: Boolean !! Any = isParallel.map(!_)
  def parallellyIf[A, U](cond: Boolean)(body: A !! U): A !! U = Primitives.configMod(_.par(cond), body)
  def sequentiallyIf[A, U](cond: Boolean)(body: A !! U): A !! U = parallellyIf(!cond)(body)
  def parallelly[A, U](body: A !! U): A !! U = parallellyIf(true)(body)
  def sequentially[A, U](body: A !! U): A !! U = parallellyIf(false)(body)


/** Alias for [[Computation]] type. Meant to be used in infix form. */
type !![+A, -U] = Computation[A, U]

/** Alias for [[Computation]] companion object.
 *  
 */
def !! = Computation

package turbolift
import turbolift.typeclass.MonadZip
import turbolift.std_effects.ChoiceSig
import turbolift.internals.effect.AnyChoice
import turbolift.internals.extensions.ComputationExtensions
import HandlerCases.Primitive
import ComputationCases._


/** Monad of extensible effects. Use `!!` infix type alias for it, to write effectful types.
  *  
  * Example:
  * {{{
  * type MyComputation = String !! (MyState & MyError)
  * }}}
  * ...is a type of computation that:
  * - Returns a `String`
  * - Reqests 2 effects:
  *     - `MyState` effect
  *     - `MyError` effect
  * 
  * All requested effects must be handled (discharged from the computation), by using [[Handler]]s, before
  * the result can be obtained as a plain (non monadic) value.
  * 
  * To handle some or all requested effects, use `handleWith` method:
  * {{{
  * val myComputation2 = myComputation.handleWith(myHandler)
  * }}}
  * 
  * As soon as all effects are handled, the result can be obtained with `run` method:
  * {{{
  * val result = someComputation
      .handleWith(someHandler1)
      .handleWith(someHandler2)
      .handleWith(someHandler3)
      .run
  * }}}
  * 
  * @tparam A Result type of the computation
  * @tparam U Type-level set of effects requested by the computation, expressed as an intersection type. `Any` denotes empty set.
  */

sealed trait Computation[+A, -U]:
  final def map[B](f: A => B): B !! U = new FlatMap(this, f andThen (new Pure(_)))
  final def flatMap[B, U2 <: U](f: A => B !! U2): B !! U2 = new FlatMap(this, f)
  final def flatten[B, U2 <: U](implicit ev: A <:< (B !! U2)): B !! U2 = flatMap(ev)
  final def zip[B, U2 <: U](that: B !! U2): (A, B) !! U2 = new Zip(this, that)
  final def flatTap[B, U2 <: U](f: A => B !! U2): A !! U2 = flatMap(a => f(a).as(a))
  final def as[B](value: B): B !! U = map(_ => value)
  final def void: Unit !! U = as(())

  final def >>=[B, U2 <: U](f: A => B !! U2): B !! U2 = flatMap(f)

  final def *![B, U2 <: U](that: B !! U2): (A, B) !! U2 = zip(that)
  final def **![B, U2 <: U](that: => B !! U2): (A, B) !! U2 = flatMap(a => that.map((a, _)))

  final def &![B, U2 <: U](that: B !! U2): B !! U2 = zip(that).map(_._2)
  final def &&![B, U2 <: U](that: => B !! U2): B !! U2 = flatMap(_ => that)

  final def &<![B, U2 <: U](that: B !! U2): A !! U2 = zip(that).map(_._1)
  final def &&<![B, U2 <: U](that: => B !! U2): A !! U2 = flatMap(a => that.map(_ => a))

  final def |![A2 >: A, U2 <: U & ChoiceSig](that: A2 !! U2): A2 !! U2 = ??? //@#@
  final def ||![A2 >: A, U2 <: U & ChoiceSig](that: => A2 !! U2): A2 !! U2 = AnyChoice.orElse(this, that)

  final def withFilter[U2 <: U & ChoiceSig](f: A => Boolean): A !! U2 = flatMap(a => if f(a) then !!.pure(a) else !!.fail)

  final def upCast[U2 <: U] = this: A !! U2

/**
  * Use `!!` alias to access methods of this companion object.
  * 
  * Example:
  * {{{
  * val myComputation: Int !! Any = !!.pure(42)
  * }}}
  */

object Computation extends ComputationExtensions with ComputationInstances:
  val unit: Unit !! Any = Pure(())
  def pure(): Unit !! Any = unit
  def pure[A](a: A): A !! Any = Pure(a)
  def defer[A, U](ua: => A !! U): A !! U = unit.flatMap(_ => ua)
  def impure[A](a: => A): A !! Any = unit.flatMap(_ => Pure(a))
  def fail: Nothing !! ChoiceSig = AnyChoice.fail

  def when[U](cond: Boolean)(body: => Unit !! U): Unit !! U = if cond then body else unit

  def unless[U](cond: Boolean)(body: => Unit !! U): Unit !! U = when(!cond)(body)

  def repeat[U](n: Int)(body: => Unit !! U): Unit !! U =
    def loop(n: Int): Unit !! U =
      if n > 0
      then body &&! loop(n - 1)
      else unit
    loop(n)

  def repeatWhile[U, U2 <: U](cond: Boolean !! U)(body: => Unit !! U2): Unit !! U2 =
    def loop: Unit !! U2 = cond.flatMap(if _ then body &&! loop else unit)
    loop

  def repeatUntil[U, U2 <: U](cond: Boolean !! U)(body: => Unit !! U2): Unit !! U2 =
    def loop: Unit !! U2 = cond.flatMap(if _ then unit else body &&! loop)
    loop

  def replicate[A, U](n: Int)(body: => A !! U): Vector[A] !! U =
    def loop(n: Int, acc: Vector[A]): Vector[A] !! U =
      if n > 0
      then body.flatMap(a => loop(n - 1, acc :+ a))
      else pure(acc)
    loop(n, Vector())

  def generate[A, B, U](init: A, cond: A => Boolean, inc: A => A)(body: A => B !! U): Vector[B] !! U =
    def loop(a: A, acc: Vector[B]): Vector[B] !! U =
      if cond(a)
      then body(a).flatMap(b => loop(inc(a), acc :+ b))
      else pure(acc)
    loop(init, Vector())

  def iterate[A, U](init: A, cond: A => Boolean, inc: A => A)(body: A => Unit !! U): A !! U =
    def loop(a: A): A !! U =
      if cond(a)
      then body(a) &&! loop(inc(a))
      else pure(a)
    loop(init)

  def iterateVoid[A, U](init: A, cond: A => Boolean, inc: A => A)(body: A => Unit !! U): Unit !! U =
    def loop(a: A): Unit !! U =
      if cond(a)
      then body(a) &&! loop(inc(a))
      else unit
    loop(init)

  def iterateWhile[A, U](init: A, cond: A => Boolean)(body: A => A !! U): A !! U =
    def loop(a: A): A !! U =
      if cond(a)
      then body(a) >>= loop
      else pure(a)
    loop(init)

  def iterateUntil[A, U](init: A, cond: A => Boolean)(body: A => A !! U): A !! U =
    iterateWhile(init, a => !cond(a))(body)


private[turbolift] object ComputationCases:
  final case class Pure[A](value: A) extends Computation[A, Any]
  final case class Lift[A, M[_], U](value: M[A]) extends Computation[A, U]
  final case class FlatMap[A, B, U](that: A !! U, k: A => B !! U) extends Computation[B, U]
  final case class Zip[A, B, U](lhs: A !! U, rhs: B !! U) extends Computation[(A, B), U]
  final case class Perform[A, U, Z <: Signature](sig: Signature, op: Z => Any) extends Computation[A, U]
  final case class Delimit[A, U, F[+_], L, N](body: A !! (U & L), handler: Primitive[F, L, N]) extends Computation[F[A], U & N]


private[turbolift] trait ComputationInstances:
  given [U]: MonadZip[Computation[_, U]] with
    override def pure[A](a: A): A !! U = Pure(a)
    override def flatMap[A, B](ua: A !! U)(f: A => B !! U): B !! U = ua.flatMap(f)
    override def zip[A, B](ua: A !! U, ub: B !! U): (A, B) !! U = ua *! ub

/** Alias for [[Computation]] type. Meant to be used in infix form. */
type !![+A, -U] = Computation[A, U]

/** Alias for [[Computation]] companion object.
 *  
 */
def !! = Computation

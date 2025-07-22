package turbolift.io
import turbolift.!!
import turbolift.data.Outcome
import turbolift.effects.IO


/** Effectful, parallelly composable result of fiber's execution. */

sealed trait Zipper[+A, -U]:
  final def zipPar[B, U2 <: U](that: Zipper[B, U2]): Zipper[(A, B), U2] = zipWithPar(that)((_, _))
  final def zipWithPar[B, C, U2 <: U](that: Zipper[B, U2])(f: (A, B) => C): Zipper[C, U2] = doZip(that.untyped, f.asInstanceOf[(Any, Any) => Any]).cast[C, U2]
  final def *![B, U2 <: U](that: Zipper[B, U2]): Zipper[(A, B), U2] = zipPar(that)
  final def &![B, U2 <: U](that: Zipper[B, U2]): Zipper[B, U2] = zipWithPar(that)((_, b) => b)
  final def &<![B, U2 <: U](that: Zipper[B, U2]): Zipper[A, U2] = zipWithPar(that)((a, _) => a)
  final def map[B](f: A => B): Zipper[B, U] = zipWithPar(this)((a, _) => f(a))
  final def as[B](b: B): Zipper[B, U] = map(_ => b)

  /** Convert back to computation. */
  def run: A !! U

  /** Inspect the `IO` component of the result, detached from the rest of effects. */
  def outcome: Outcome[Unit]

  /** Convert to pure value, provided the effect set is empty. */
  def get(using Any <:< U): A

  /** Try to convert to a single pure value.
    *
    * If the effect set includes nondeterminism (e.g. `Choice`),
    * then at most one branch is taken.
    */
  def toOption: Option[A]

  /** Like [[get]], provided the effect set is a singleton of `IO`. */
  def getIO(using IO <:< U): Outcome[A]

  private[turbolift] def doHandleIO[V]: Outcome[Zipper[A, V]]
  private[turbolift] def doZip(that: Zipper.Untyped, f: (Any, Any) => Any): Zipper.Untyped

  private[turbolift] final def untyped: Zipper.Untyped = asInstanceOf[Zipper.Untyped]
  private[turbolift] final def cast[A2, U2]: Zipper[A2, U2] = asInstanceOf[Zipper[A2, U2]]


object Zipper:
  type Untyped = Zipper[Any, Nothing]
  private[turbolift] trait Unsealed extends Untyped

  extension [A, U](thiz: Zipper[A, U & IO])
    /** Eliminate `IO` from the effect set. */
    def handleIO: Outcome[Zipper[A, U]] = thiz.doHandleIO

package turbolift.data
import turbolift.!!
import turbolift.effects.IO
import turbolift.interpreter.Prompt
import turbolift.internals.engine.stacked.Prompt
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import Snap.{Success, Failure}


/** Like [[Outcome]], but the cause hase 1 extra case: `Aborted`.
 *
 * [[Outcome]] is the final result of a completed fiber.
 * [[Snap]] is a "snapshot" of a pending fiber's execution:
 * it's either computing a value, or unwinding the stack.
 */

sealed abstract class Snap[+A]:
  final def map[B](f: A => B): Snap[B] =
    this match
      case Success(a) => Success(f(a))
      case x: Failure => x

  final def flatMap[B](f: A => Snap[B]): Snap[B] =
    this match
      case Success(a) => f(a)
      case x: Failure => x

  final def get: A =
    this match
      case Success(a) => a
      case Failure(c) => throw c.toThrowable

  final def isSuccess: Boolean =
    this match
      case Success(_) => true
      case _ => false

  final def isFailure: Boolean =
    this match
      case Failure(_) => true
      case _ => false

  final def isCancelled: Boolean =
    this match
      case Failure(Cause.Cancelled) => true
      case _ => false

  final def isThrown: Boolean =
    this match
      case Failure(Cause.Thrown(_)) => true
      case _ => false

  final def toTry: Try[A] =
    this match
      case Success(a) => TrySuccess(a)
      case Failure(c) => TryFailure(c.toThrowable)

  final def toEither: Either[Throwable, A] =
    this match
      case Success(a) => Right(a)
      case Failure(c) => Left(c.toThrowable)

  final def toOption: Option[A] =
    this match
      case Success(a) => Some(a)
      case _ => None

  final def toOutcome: Outcome[A] =
    this match
      case Success(a) => Outcome.Success(a)
      case Failure(c) => Outcome.Failure(c)

  final def run: A !! IO = IO.unsnap(this)


object Snap:
  val unit: Snap[Unit] = Snap.Success(())

  final case class Success[A](value: A) extends Snap[A]
  final case class Failure(cause: Cause) extends Snap[Nothing]

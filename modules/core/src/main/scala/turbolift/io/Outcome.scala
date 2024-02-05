package turbolift.io
import turbolift.!!
import turbolift.effects.IO
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import Outcome.{Success, Failure, Cancelled, NotSuccess}


sealed abstract class Outcome[+A]:
  final def map[B](f: A => B): Outcome[B] =
    this match
      case Success(a) => Success(f(a))
      case x: NotSuccess => x


  final def flatMap[B](f: A => Outcome[B]): Outcome[B] =
    this match
      case Success(a) => f(a)
      case x: NotSuccess => x


  final def get: A =
    this match
      case Success(a) => a
      case Failure(_) => sys.error("Fiber failed.")
      case Cancelled => sys.error("Fiber cancelled.")


  final def isFailure: Boolean =
    this match
      case Failure(_) => true
      case _ => false


  final def isSuccess: Boolean =
    this match
      case Success(_) => true
      case _ => false


  final def isCancelled: Boolean =
    this match
      case Cancelled => true
      case _ => false


  final def toSnap: Snap[A] =
    this match
      case Success(a) => Snap.Success(a)
      case Failure(c) => Snap.Failure(c)
      case Cancelled => Snap.Cancelled


  final def toTry: Try[A] =
    this match
      case Success(a) => TrySuccess(a)
      case Failure(c) => TryFailure(c.last)
      case Cancelled => TryFailure(Exceptions.Cancelled)


  final def toOption: Option[A] = toTry.toOption
  final def toEither: Either[Throwable, A] = toTry.toEither


  final def run: A !! IO =
    this match
      case Success(a) => !!.pure(a)
      case Failure(c) => IO.fail(c)
      case Cancelled => IO.cancel


  final def flatRun[B, U](using ev: A <:< (B !! U)): B !! (U & IO) =
    this match
      case Success(a) => ev(a)
      case Failure(c) => IO.fail(c)
      case Cancelled => IO.cancel


object Outcome:
  def fromTry[A](aa: Try[A]): Outcome[A] =
    aa match
      case TrySuccess(a) => Outcome.Success(a)
      case TryFailure(Exceptions.Cancelled) => Outcome.Cancelled
      case TryFailure(e) => Outcome.Failure(e)


  final case class Success[A](value: A) extends Outcome[A]

  sealed abstract class NotSuccess extends Outcome[Nothing]
  case object Cancelled extends NotSuccess

  final case class Failure(cause: Cause) extends NotSuccess
  object Failure:
    def apply(e: Throwable): Failure = Failure(Cause(e))

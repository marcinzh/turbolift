package turbolift.data
import turbolift.!!
import turbolift.effects.IO
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import Outcome.{Success, Failure}


sealed abstract class Outcome[+A]:
  final def map[B](f: A => B): Outcome[B] =
    this match
      case Success(a) => Success(f(a))
      case x: Failure => x

  final def flatMap[B](f: A => Outcome[B]): Outcome[B] =
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

  final def toOption: Option[A] = toEither.toOption
    this match
      case Success(a) => Some(a)
      case _ => None

  final def toSnap: Snap[A] =
    this match
      case Success(a) => Snap.Success(a)
      case Failure(c) => Snap.Failure(c)

  final def run: A !! IO =
    this match
      case Success(a) => !!.pure(a)
      case Failure(c) => IO.fail(c)

  final def flatRun[B, U](using ev: A <:< (B !! U)): B !! (U & IO) =
    this match
      case Success(a) => ev(a)
      case Failure(c) => IO.fail(c)


object Outcome:
  val unit: Outcome[Unit] = Outcome.Success(())

  def fromTry[A](aa: Try[A]): Outcome[A] =
    aa match
      case TrySuccess(a) => Outcome.Success(a)
      case TryFailure(e) => Outcome.Failure(e)

  def fromEither[A](aa: Either[Throwable, A]): Outcome[A] =
    aa match
      case Right(a) => Outcome.Success(a)
      case Left(e) => Outcome.Failure(e)


  final case class Success[A](value: A) extends Outcome[A]
  final case class Failure(cause: Cause) extends Outcome[Nothing]

  val Cancelled = Failure(Cause.Cancelled)

  object Failure:
    def apply(e: Throwable): Failure = Failure(Cause(e))

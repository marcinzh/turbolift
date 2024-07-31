package turbolift.io
import turbolift.!!
import turbolift.effects.IO
import turbolift.internals.engine.Prompt
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}


sealed abstract class Snap[+A]:
  final def toOutcome: Outcome[A] =
    (this: @unchecked) match
      case Snap.Success(a) => Outcome.Success(a)
      case Snap.Failure(c) => Outcome.Failure(c)
      case Snap.Cancelled => Outcome.Cancelled
      case Snap.Aborted(a) => Outcome.Failure(Cause.Thrown(Exceptions.Aborted(a)))

  final def toTry: Try[A] =
    (this: @unchecked) match
      case Snap.Success(a) => TrySuccess(a)
      case Snap.Failure(c) => c.toTry
      case Snap.Cancelled => TryFailure(Exceptions.Cancelled)
      case Snap.Aborted(a) => TryFailure(Exceptions.Aborted(a))

  final def toEither: Either[Throwable, A] =
    (this: @unchecked) match
      case Snap.Success(a) => Right(a)
      case Snap.Failure(c) => c.toEither
      case Snap.Cancelled => Left(Exceptions.Cancelled)
      case Snap.Aborted(a) => Left(Exceptions.Aborted(a))

  final def run: A !! IO = IO.unsnap(this)


object Snap:
  val unit: Snap[Unit] = Snap.Success(())

  def fromOutcome[A](aa: Outcome[A]): Snap[A] = aa.toSnap

  sealed abstract class NotSuccess extends Snap[Nothing]
  final case class Success[A](value: A) extends Snap[A]
  final case class Failure(cause: Cause) extends NotSuccess
  case object Cancelled extends NotSuccess

  final case class Aborted private[turbolift](val value: Any, private[turbolift] val prompt: Prompt) extends NotSuccess
  object Aborted:
    def unapply(x: Aborted): Option[Any] = Some(x.value)

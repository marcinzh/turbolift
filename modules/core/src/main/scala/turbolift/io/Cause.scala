package turbolift.io
import scala.util.{Try, Failure => TryFailure}


sealed abstract class Cause:
  final def toTry: Try[Nothing] = TryFailure(last)
  final def toEither: Either[Throwable, Nothing] = Left(last)
  final def ++(that: Cause): Cause = Cause.Then(this, that)
  final def &(that: Cause): Cause = Cause.Both(this, that)
  
  final def last: Throwable =
    this match
      case Cause.Thrown(x) => x
      case Cause.Then(_, x) => x.last
      case Cause.Both(_, x) => x.last
      case Cause.Cancelled => Exceptions.Cancelled
      case Cause.Aborted(x) => Exceptions.Aborted(x)

  final def all: Vector[Throwable] =
    this match
      case Cause.Thrown(x) => Vector(x)
      case Cause.Then(x, y) => x.all ++ y.all
      case Cause.Both(x, y) => x.all ++ y.all
      case Cause.Cancelled => Vector(Exceptions.Cancelled)
      case Cause.Aborted(x) => Vector(Exceptions.Aborted(x))

object Cause:
  def apply(e: Throwable): Cause = Thrown(e)

  case object Cancelled extends Cause
  case class Thrown(throwable: Throwable) extends Cause
  case class Aborted(value: Any) extends Cause
  case class Then(left: Cause, right: Cause) extends Cause
  case class Both(left: Cause, right: Cause) extends Cause

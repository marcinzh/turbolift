package turbolift.io
import scala.util.{Try, Failure => TryFailure}


sealed abstract class Cause extends Cause.NonPrimary:
  final def toTry: Try[Nothing] = TryFailure(last)
  final def toEither: Either[Throwable, Nothing] = Left(last)
  

object Cause:
  def apply(e: Throwable): Cause = Thrown(e)

  sealed abstract class NonPrimary:
    final def :+(that: Cause): Cause = Then(this, that)
  
    final def last: Throwable =
      this match
        case Thrown(x) => x
        case Then(_, x) => x.last
        case Cancelled => Exceptions.Cancelled
        case Aborted(x) => Exceptions.Aborted(x)

    final def all: Vector[Throwable] =
      this match
        case Thrown(x) => Vector(x)
        case Then(x, y) => x.all ++ y.all
        case Cancelled => Vector(Exceptions.Cancelled)
        case Aborted(x) => Vector(Exceptions.Aborted(x))

  case class Thrown(throwable: Throwable) extends Cause
  case class Then(left: NonPrimary, right: Cause) extends Cause

  case object Cancelled extends NonPrimary
  case class Aborted(value: Any) extends NonPrimary

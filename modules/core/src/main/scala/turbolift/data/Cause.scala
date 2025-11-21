package turbolift.data
import turbolift.interpreter.Prompt
import scala.util.{Try, Failure => TryFailure}


sealed abstract class Cause:
  final def toTry: Try[Nothing] = TryFailure(toThrowable)
  final def toEither: Either[Throwable, Nothing] = Left(toThrowable)

  final def toThrowable: Throwable =
    this match
      case Cause.Thrown(x) => x
      case Cause.Cancelled => Exceptions.Cancelled
      //@#@WTF Error: unreachable case
      // case Aborted(x, p) => Exceptions.Aborted(x, p)
      case c: Cause.Aborted => Exceptions.Aborted(c.value, c.prompt)
      case Cause.Then(a, b) => b.toThrowable
      case Cause.Both(a, b) => b.toThrowable

  final def &&(that: Cause): Cause = Cause.Then(this, that)
  final def ||(that: Cause): Cause = Cause.Both(this, that)


object Cause:
  def apply(e: Throwable): Cause = Thrown(e)


  case object Cancelled extends Cause
  final case class Thrown(throwable: Throwable) extends Cause
  final case class Aborted(value: Any, prompt: Prompt) extends Cause
  final case class Then(left: Cause, right: Cause) extends Cause
  final case class Both(left: Cause, right: Cause) extends Cause

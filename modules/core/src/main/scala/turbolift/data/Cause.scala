package turbolift.data
import turbolift.interpreter.Prompt
import scala.util.{Try, Failure => TryFailure}


sealed abstract class Cause:
  final def ||(that: Cause): Cause = Cause.Both(this, that)
  final def &&(that: Cause): Cause = Cause.Then(this, that)
  final def ^^(that: Cause): Cause = Cause.Then(that, this)

  final def last: Cause.Singular =
    this match
      case cause: Cause.Singular => cause
      case Cause.Both(_, that) => that.last
      case Cause.Then(_, that) => that.last

  final def split: (Cause.Singular, Option[Cause]) =
    this match
      case cause: Cause.Singular => (cause, None)
      case Cause.Both(_, _) => (Cause.Ambiguous, Some(this))
      case Cause.Then(t1, t2) =>
        val (c, ot3) = t2.split
        (c, Some(ot3.fold(t1)(t1 && _)))

  final def toSnap: Snap[Nothing] =
    val (c, s) = split
    Snap.Failure(c, s)


object Cause:
  def apply(e: Throwable): Singular = Thrown(e)

  case object Cancelled extends Singular
  final case class Thrown(throwable: Throwable) extends Singular
  final case class Aborted(value: Any, prompt: Prompt) extends Singular
  final case class Both(left: Cause, right: Cause) extends Cause
  final case class Then(left: Cause, right: Cause) extends Cause

  val Ambiguous = Thrown(Exceptions.AmbiguousCause)


  sealed abstract class Singular extends Cause:
    final def toTry: Try[Nothing] = TryFailure(toThrowable)
    final def toEither: Either[Throwable, Nothing] = Left(toThrowable)

    final def toThrowable: Throwable =
      this match
        case Thrown(x) => x
        case Cancelled => Exceptions.Cancelled
        //@#@WTF Error: unreachable case
        // case Aborted(x, p) => Exceptions.Aborted(x, p)
        case c: Aborted => Exceptions.Aborted(c.value, c.prompt)

    final def +>(suppressed: Option[Cause]): Cause =
      (this, suppressed) match
        case (Ambiguous, Some(p: Both)) => p
        case (c, None) => c
        case (c, Some(t)) => t && c

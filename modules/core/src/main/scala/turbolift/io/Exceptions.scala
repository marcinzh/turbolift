package turbolift.io
import scala.util.control.ControlThrowable
import turbolift.interpreter.Prompt


object Exceptions:
  case object Cancelled extends ControlThrowable("Fiber cancelled.")

  final class Pending extends RuntimeException("Fiber pending.")
  
  final class Aborted(value: Any, prompt: Prompt) extends ControlThrowable

  final class Unhandled(throwable: Throwable) extends RuntimeException(throwable)

  final class TieTheKnot extends RuntimeException

  final class Panic(msg: String) extends RuntimeException(msg)

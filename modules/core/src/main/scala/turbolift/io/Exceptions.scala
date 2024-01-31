package turbolift.io
import scala.util.control.ControlThrowable


object Exceptions:
  case object Cancelled extends ControlThrowable("Fiber cancelled.")

  final class Aborted(value: Any) extends ControlThrowable

  final class Unhandled(throwable: Throwable) extends RuntimeException(throwable)

  final class Panic(msg: String) extends RuntimeException(msg)

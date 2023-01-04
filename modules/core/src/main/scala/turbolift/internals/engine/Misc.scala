package turbolift.internals.engine
import turbolift.!!


private[engine] type AnyComp = Any !! Any


private[internals] final class Panic(m: String, c: Throwable | Null = null) extends RuntimeException(m, c)

private[internals] case object Cancelled extends Throwable("Fiber cancelled", null, false, false):
  override def setStackTrace(x: Array[StackTraceElement]): Unit = ()

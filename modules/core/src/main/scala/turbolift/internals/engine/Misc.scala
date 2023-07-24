package turbolift.internals.engine
import scala.util.Try
import turbolift.!!


private[engine] type AnyComp = Any !! Any

private[engine] type Owner = FiberImpl | AnyCallback | Null

private[internals] type AnyCallback = Try[Any] => Unit

private[internals] final class Panic(m: String, c: Throwable | Null = null) extends RuntimeException(m, c)

private[internals] def impossible: Nothing = throw new Panic("impossible happened")

private[internals] case object Cancelled extends Throwable("Fiber cancelled", null, false, false):
  override def setStackTrace(x: Array[StackTraceElement]): Unit = ()


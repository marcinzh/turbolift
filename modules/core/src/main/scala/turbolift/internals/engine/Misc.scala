package turbolift.internals.engine
import turbolift.!!
import turbolift.io.{Outcome, Exceptions, Snap}
import turbolift.interpreter.{Interpreter, Continuation}


private[engine] type AnyComp = Any !! Any
private[engine] type Owner = FiberImpl | AnyCallback | Null
private[internals] type AnyCallback = Outcome[Any] => Unit

private[engine] def panic(msg: String): Nothing = throw new Exceptions.Panic(msg)
private[engine] def impossible: Nothing = panic("impossible happened")

extension (thiz: Continuation[?, ?, ?, ?])
  private[engine] inline def asImpl: ContImpl = thiz.asInstanceOf[ContImpl]
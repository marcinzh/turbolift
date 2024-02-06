package turbolift.internals.engine
import turbolift.!!
import turbolift.io.{Outcome, Exceptions, Snap}


private[engine] type AnyComp = Any !! Any
private[engine] type Owner = FiberImpl | AnyCallback | Null
private[engine] type GuardFunc[A, B, U] = Snap[A] => B !! U
private[engine] type AnyGuardFunc = GuardFunc[Any, Any, Any]
private[internals] type AnyCallback = Outcome[Any] => Unit

private[engine] inline val YIELD = -1

private[engine] def panic(msg: String): Nothing = throw new Exceptions.Panic(msg)
private[engine] def impossible: Nothing = panic("impossible happened")
private[engine] def despair: Nothing =
  panic:
    "Stack lazily splits. " +
    "Assumptions crumble, betrayed. " +
    "Sanity check laments." 

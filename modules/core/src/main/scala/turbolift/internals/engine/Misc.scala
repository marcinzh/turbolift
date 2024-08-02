package turbolift.internals.engine
import turbolift.!!
import turbolift.io.{Outcome, Exceptions}
import turbolift.internals.engine.concurrent.{FiberImpl, WarpImpl}


//@#@
// export Misc._

private[engine] object Misc:
  type AnyComp = Any !! Any
  type Owner = FiberImpl | WarpImpl | AnyCallback
  type AnyCallback = Outcome[Any] => Unit

  def panic(msg: String): Nothing = throw new Exceptions.Panic(msg)
  def impossible: Nothing = panic("impossible happened")

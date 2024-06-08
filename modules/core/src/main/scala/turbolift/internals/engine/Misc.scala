package turbolift.internals.engine
import turbolift.!!
import turbolift.io.{Outcome, Exceptions, Snap, Fiber, Zipper, Warp, OnceVarGet}
import turbolift.interpreter.Continuation


private[engine] type AnyComp = Any !! Any
private[engine] type Owner = FiberImpl | WarpImpl | AnyCallback
private[internals] type AnyCallback = Outcome[Any] => Unit

private[engine] def panic(msg: String): Nothing = throw new Exceptions.Panic(msg)
private[engine] def impossible: Nothing = panic("impossible happened")

extension (thiz: Continuation[?, ?, ?, ?])
  private[engine] inline def asImpl: ContImpl = thiz.asInstanceOf[ContImpl]

extension (thiz: Fiber[?, ?])
  private[engine] inline def asImpl: FiberImpl = thiz.asInstanceOf[FiberImpl]

extension (thiz: Zipper[?, ?])
  private[engine] inline def asImpl: ZipperImpl = thiz.asInstanceOf[ZipperImpl]

extension (thiz: Warp)
  private[engine] inline def asImpl: WarpImpl = thiz.asInstanceOf[WarpImpl]

extension (thiz: OnceVarGet[?])
  private[engine] inline def asImpl: OnceVarImpl = thiz.asInstanceOf[OnceVarImpl]

extension (thiz: Boolean)
  private[engine] def toInt: Int = if thiz then 1 else 0

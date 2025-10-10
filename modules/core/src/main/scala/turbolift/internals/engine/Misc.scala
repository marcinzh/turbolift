package turbolift.internals.engine
import turbolift.!!
import turbolift.data.{Outcome, Exceptions}
import turbolift.io.{Fiber, Zipper, Warp}
import turbolift.interpreter.Continuation


//@#@
// export Misc._

private[engine] object Misc:
  type AnyComp = Any !! Nothing
  type Owner = FiberImpl | WarpImpl | AnyCallback
  type AnyCallback = Outcome[Any] => Unit

  def panic(msg: String): Nothing = throw new Exceptions.Panic(msg)
  def impossible: Nothing = panic("impossible happened")

  def nullableToOption[T](x: T | Null): Option[T] = if x != null then Some(x) else None



  extension (thiz: Continuation[?, ?, ?, ?])
    private[engine] inline def asImpl: ContImpl = thiz.asInstanceOf[ContImpl]

  extension (thiz: Fiber[?, ?])
    private[engine] inline def asImpl: FiberImpl = thiz.asInstanceOf[FiberImpl]

  extension (thiz: Zipper[?, ?])
    private[engine] inline def asImpl: ZipperImpl = thiz.asInstanceOf[ZipperImpl]

  extension (thiz: Warp)
    private[engine] inline def asImpl: WarpImpl = thiz.asInstanceOf[WarpImpl]

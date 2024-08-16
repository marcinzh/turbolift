package turbolift.internals.engine
import turbolift.io.{Fiber, Zipper, Warp}
import turbolift.interpreter.Continuation
import turbolift.internals.engine.concurrent.{FiberImpl, WarpImpl, ZipperImpl}


extension (thiz: Continuation[?, ?, ?, ?])
  private[engine] inline def asImpl: ContImpl = thiz.asInstanceOf[ContImpl]

extension (thiz: Fiber[?, ?])
  private[engine] inline def asImpl: FiberImpl = thiz.asInstanceOf[FiberImpl]

extension (thiz: Zipper[?, ?])
  private[engine] inline def asImpl: ZipperImpl = thiz.asInstanceOf[ZipperImpl]

extension (thiz: Warp)
  private[engine] inline def asImpl: WarpImpl = thiz.asInstanceOf[WarpImpl]

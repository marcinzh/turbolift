package turbolift.internals.engine
import turbolift.io.{Fiber, Zipper, Warp, OnceVar}
import turbolift.interpreter.Continuation
import turbolift.internals.engine.concurrent.{FiberImpl, WarpImpl, ZipperImpl, OnceVarImpl}


extension (thiz: Continuation[?, ?, ?, ?])
  private[engine] inline def asImpl: ContImpl = thiz.asInstanceOf[ContImpl]

extension (thiz: Fiber[?, ?])
  private[engine] inline def asImpl: FiberImpl = thiz.asInstanceOf[FiberImpl]

extension (thiz: Zipper[?, ?])
  private[engine] inline def asImpl: ZipperImpl = thiz.asInstanceOf[ZipperImpl]

extension (thiz: Warp)
  private[engine] inline def asImpl: WarpImpl = thiz.asInstanceOf[WarpImpl]

extension (thiz: OnceVar.Get[?])
  private[engine] inline def asImpl: OnceVarImpl = thiz.asInstanceOf[OnceVarImpl]

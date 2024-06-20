package turbolift.internals.engine
import turbolift.!!
import turbolift.io.{Outcome, Exceptions, Snap, Fiber, Zipper, Warp, OnceVarGet}
import turbolift.interpreter.Continuation


export turbolift.interpreter.Interpreter.{Untyped => Prompt}
export turbolift.interpreter.Interpreter.Io.{untyped => PromptIO}


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

extension (thiz: Prompt)
  private[engine] inline def localCount: Int = if thiz.features.isStateful then 1 else 0
  private[engine] inline def isIo = thiz.features.isIo
  private[engine] inline def isStateful = thiz.features.isStateful
  private[engine] inline def isStateless = thiz.features.isStateless
  private[engine] inline def isParallel = thiz.features.isParallel
  private[engine] inline def hasRestart = thiz.features.hasRestart
  private[engine] inline def hasZip = thiz.features.hasZip
  private[engine] inline def hasForkJoin = thiz.features.hasForkJoin
  private[engine] def unwind: Step = StepCases.Unwind(Step.UnwindKind.Abort, thiz)

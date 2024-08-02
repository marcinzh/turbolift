package turbolift.internals.engine.stacked
import turbolift.interpreter.Interpreter


private[turbolift] type Prompt = Interpreter.Untyped
//@#@
// private[turbolift] opaque type Prompt <: AnyRef = Interpreter.Untyped

private[engine] object Prompt:
  export Interpreter.Io.{untyped => IO}
  // def IO: Prompt = Interpreter.Io.untyped

  object Syntax:
    extension (thiz: Prompt)
      // inline def unwrap: Interpreter.Untyped = thiz
      inline def localCount: Int = if thiz.features.isStateful then 1 else 0
      //@#@NNEC?
      inline def isIo = thiz.features.isIo
      inline def isStateful = thiz.features.isStateful
      inline def isStateless = thiz.features.isStateless
      inline def isParallel = thiz.features.isParallel
      inline def hasRestart = thiz.features.hasRestart
      inline def hasZip = thiz.features.hasZip
      inline def hasForkJoin = thiz.features.hasForkJoin
      def unwind: Step = StepCases.Unwind(Step.UnwindKind.Abort, thiz)

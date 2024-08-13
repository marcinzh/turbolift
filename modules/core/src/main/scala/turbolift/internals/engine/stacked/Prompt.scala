package turbolift.internals.engine.stacked
import turbolift.interpreter.Interpreter


private[turbolift] type Prompt = Interpreter.Untyped

private[engine] object Prompt:
  export Interpreter.Io.{untyped => IO}

  object Syntax:
    extension (thiz: Prompt)
      inline def localCount: Int = if thiz.features.isStateful then 1 else 0
      inline def isIo = thiz.features.isIo
      inline def isStateful = thiz.features.isStateful
      inline def isStateless = thiz.features.isStateless
      inline def isParallel = thiz.features.isParallel
      inline def hasRestart = thiz.features.hasRestart
      inline def hasZip = thiz.features.hasZip
      inline def hasForkJoin = thiz.features.hasForkJoin
      def unwind: Step = StepCases.Unwind(Step.UnwindKind.Abort, thiz)

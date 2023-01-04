package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.{!!, Signature}
import turbolift.internals.interpreter.Interpreter

private[internals] final class Prompt(
  val signature: Signature,
  val flow: Interpreter.FlowUntyped,
  val storeIndex: Short,
  val levelIndex: Short,
  val isParallelizable: Boolean,
  canAbort: Boolean,
):
  val abort: Step =
    // if isFlow
    if canAbort
    then StepCases.Abort(this)
    else null.asInstanceOf[Step]

  def isFlow: Boolean = flow ne null
  def isGlobal: Boolean = signature eq null
  def isChoice: Boolean = anyInterpreter.isChoice
  def hasStan: Boolean = storeIndex >= 0
  def anyInterpreter = signature.asInstanceOf[Interpreter.Untyped]

  def pure(value: Any, stan: Any): Any = flow.onPure(value, stan)

  override def toString = s"Prompt(${toStr})"
  def toStr = if isGlobal then "Global" else anyInterpreter.signatures.head.toString


private[engine] object Prompt:
  def create(topPrompt: Prompt, interpreter: Interpreter, nextStoreIndex: Int): Prompt =
    if interpreter.isFlow then
      val flow = interpreter.asInstanceOf[Interpreter.FlowUntyped]
      new Prompt(
        signature = flow,
        flow = flow,
        storeIndex = if flow.isStateful then nextStoreIndex.toShort else -1,
        levelIndex = (topPrompt.levelIndex + 1).toShort,
        isParallelizable = topPrompt.isParallelizable && flow.isParallelizable,
        canAbort = true,
      )
    else
      new Prompt(
        signature = interpreter,
        flow = null.asInstanceOf[Interpreter.FlowUntyped],
        storeIndex = -1,
        levelIndex = -1,
        isParallelizable = topPrompt.isParallelizable,
        canAbort = false,
      )

  val global: Prompt =
    new Prompt(
      signature = null.asInstanceOf[Signature],
      flow = null.asInstanceOf[Interpreter.FlowUntyped],
      storeIndex = -1,
      levelIndex = -1,
      isParallelizable = true,
      canAbort = true,
    )

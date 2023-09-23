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
  val isProxyIO: Boolean,
  belowLookup: AnyRef,
  canAbort: Boolean,
):
  val abort: Step =
    // if isFlow
    if canAbort
    then StepCases.Abort(this)
    else null.asInstanceOf[Step]

  val below: Lookup = belowLookup.asInstanceOf[Lookup]

  def isFlow: Boolean = flow ne null
  def isGlobal: Boolean = signature eq null
  def isChoice: Boolean = anyInterpreter.isChoice
  def hasStan: Boolean = storeIndex >= 0
  def anyInterpreter = signature.asInstanceOf[Interpreter.Untyped]

  def pure(value: Any, stan: Any): AnyComp = flow.onReturn(value, stan)

  override def toString = s"Prompt(#${##.toHexString} ${toStr} l=$levelIndex  s=$storeIndex)"
  def toStr = if isGlobal then "Global" else anyInterpreter.signatures.head.toString


private[engine] object Prompt:
  def create(lookup: Lookup, interpreter: Interpreter, nextStoreIndex: Int, nextLevelIndex: Int): Prompt =
    val isPar = lookup.top.isParallelizable
    if interpreter.isFlow then
      val flow = interpreter.asInstanceOf[Interpreter.FlowUntyped]
      new Prompt(
        signature = flow,
        flow = flow,
        storeIndex = if flow.isStateful then nextStoreIndex.toShort else -1,
        levelIndex = nextLevelIndex.toShort,
        isParallelizable = isPar && flow.isParallelizable,
        isProxyIO = false,
        belowLookup = lookup,
        canAbort = true,
      )
    else
      new Prompt(
        signature = interpreter,
        flow = null.asInstanceOf[Interpreter.FlowUntyped],
        storeIndex = -1,
        levelIndex = -1,
        isParallelizable = isPar,
        isProxyIO = interpreter.isProxyIO,
        belowLookup = lookup,
        canAbort = false,
      )

  val global: Prompt =
    new Prompt(
      signature = null.asInstanceOf[Signature],
      flow = null.asInstanceOf[Interpreter.FlowUntyped],
      storeIndex = -1,
      levelIndex = -1,
      belowLookup = Lookup.initial,
      isParallelizable = true,
      isProxyIO = false,
      canAbort = true,
    )

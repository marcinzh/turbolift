package turbolift.internals.engine
import turbolift.Signature
import turbolift.interpreter.Interpreter
import turbolift.internals.executor.Executor


private[internals] final class Env(
  val prompt: Prompt,
  val executor: Executor,
  val tickLow: Short,
  val tickHigh: Short,
  val isParallelismRequested: Boolean = true,
):
  def copy(
    //// prompt never changes
    executor: Executor = executor,
    tickLow: Short = tickLow,
    tickHigh: Short = tickHigh,
    isParallelismRequested: Boolean = isParallelismRequested,
  ) = new Env(
    prompt = prompt,
    executor = executor,
    tickLow = tickLow,
    tickHigh = tickHigh,
    isParallelismRequested = isParallelismRequested,
  )

  def par(x: Boolean): Env =
    if isParallelismRequested == x then
      this
    else
      copy(isParallelismRequested = x)

  override def toString = "Env"


private[internals] object Env:
  def default(executor: Executor): Env =
    new Env(
      prompt = new Prompt(interpreter),
      executor = executor,
      tickHigh = 10,
      tickLow = 500,
    )

  def interpreter: Interpreter.Untyped = Interpreter.Root.untyped
  val signature: Signature = interpreter.signatures.head

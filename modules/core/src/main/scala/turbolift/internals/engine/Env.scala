package turbolift.internals.engine
import turbolift.Signature
import turbolift.interpreter.Interpreter
import turbolift.internals.executor.Executor


private[turbolift] final class Env(
  val executor: Executor,
  val tickLow: Short,
  val tickHigh: Short,
  val isParallelismRequested: Boolean = true,
  val currentWarp: WarpImpl | Null,
):
  def copy(
    executor: Executor = executor,
    tickLow: Short = tickLow,
    tickHigh: Short = tickHigh,
    isParallelismRequested: Boolean = isParallelismRequested,
    currentWarp: WarpImpl | Null = currentWarp,
  ) = new Env(
    executor = executor,
    tickLow = tickLow,
    tickHigh = tickHigh,
    isParallelismRequested = isParallelismRequested,
    currentWarp = currentWarp,
  )

  def par(x: Boolean): Env =
    if isParallelismRequested == x then
      this
    else
      copy(isParallelismRequested = x)

  override def toString = "Env"


private[internals] object Env:
  def initial(executor: Executor): Env =
    new Env(
      executor = executor,
      tickHigh = 20,
      tickLow = 1000,
      currentWarp = null,
  )

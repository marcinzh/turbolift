package turbolift.internals.engine
import turbolift.Signature
import turbolift.interpreter.Interpreter
import turbolift.internals.executor.Resumer


private[turbolift] final class Env(
  val resumer: Resumer,
  val tickLow: Short,
  val tickHigh: Short,
  val isParallelismRequested: Boolean = true,
  val currentWarp: WarpImpl,
  val initialWarp: WarpImpl,
):
  def copy(
    resumer: Resumer = resumer,
    tickLow: Short = tickLow,
    tickHigh: Short = tickHigh,
    isParallelismRequested: Boolean = isParallelismRequested,
    currentWarp: WarpImpl = currentWarp,
    initialWarp: WarpImpl = initialWarp,
  ) = new Env(
    resumer = resumer,
    tickLow = tickLow,
    tickHigh = tickHigh,
    isParallelismRequested = isParallelismRequested,
    currentWarp = currentWarp,
    initialWarp = initialWarp,
  )

  def par(x: Boolean): Env =
    if isParallelismRequested == x then
      this
    else
      copy(isParallelismRequested = x)

  override def toString = "Env"


private[internals] object Env:
  def initial(warp: WarpImpl, resumer: Resumer): Env =
    new Env(
      resumer = resumer,
      tickHigh = 20,
      tickLow = 1000,
      currentWarp = warp,
      initialWarp = warp,
  )

package turbolift.internals.engine
import turbolift.Signature
import turbolift.interpreter.Interpreter
import turbolift.internals.executor.Executor
import turbolift.internals.engine.concurrent.WarpImpl


private[turbolift] final class Env(
  val executor: Executor,
  val tickLow: Short,
  val tickHigh: Short,
  val currentWarp: WarpImpl | Null,
  val shadowMap: ShadowMap,
  val isParallelismRequested: Boolean = true,
  val isCancellable: Boolean,
):
  def copy(
    executor: Executor = executor,
    tickLow: Short = tickLow,
    tickHigh: Short = tickHigh,
    currentWarp: WarpImpl | Null = currentWarp,
    shadowMap: ShadowMap = shadowMap,
    isParallelismRequested: Boolean = isParallelismRequested,
    isCancellable: Boolean = isCancellable
  ) = new Env(
    executor = executor,
    tickLow = tickLow,
    tickHigh = tickHigh,
    currentWarp = currentWarp,
    shadowMap = shadowMap,
    isParallelismRequested = isParallelismRequested,
    isCancellable = isCancellable,
  )

  def par(x: Boolean): Env =
    if isParallelismRequested == x then
      this
    else
      copy(isParallelismRequested = x)

  override def toString = s"Env#%04X".format(hashCode & 0xFFFF)



private[internals] object Env:
  def initial(executor: Executor): Env =
    new Env(
      executor = executor,
      tickHigh = 20,
      tickLow = 1000,
      currentWarp = null,
      shadowMap = ShadowMap.empty,
      isCancellable = true,
    )

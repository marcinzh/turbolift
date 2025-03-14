package turbolift.internals.engine
import turbolift.Signature
import turbolift.interpreter.Interpreter
import turbolift.internals.executor.Executor
import turbolift.internals.engine.concurrent.WarpImpl


private[turbolift] final class Env(
  val executor: Executor,
  val tickLow: Short,
  val tickHigh: Short,
  val isParallelismRequested: Boolean = true,
  val currentWarp: WarpImpl | Null,
  val suppressions: Int,
  val shadowMap: ShadowMap,
):
  def copy(
    executor: Executor = executor,
    tickLow: Short = tickLow,
    tickHigh: Short = tickHigh,
    isParallelismRequested: Boolean = isParallelismRequested,
    currentWarp: WarpImpl | Null = currentWarp,
    suppressions: Int = suppressions,
    shadowMap: ShadowMap = shadowMap,
  ) = new Env(
    executor = executor,
    tickLow = tickLow,
    tickHigh = tickHigh,
    isParallelismRequested = isParallelismRequested,
    currentWarp = currentWarp,
    suppressions = suppressions,
    shadowMap = shadowMap,
  )

  def par(x: Boolean): Env =
    if isParallelismRequested == x then
      this
    else
      copy(isParallelismRequested = x)

  override def toString = s"Env#%04X".format(hashCode & 0xFFFF)

  def isCancellable: Boolean = suppressions <= 0


private[internals] object Env:
  def initial(executor: Executor): Env =
    new Env(
      executor = executor,
      tickHigh = 20,
      tickLow = 1000,
      currentWarp = null,
      suppressions = 0,
      shadowMap = ShadowMap.empty,
    )

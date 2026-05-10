package turbolift.internals.engine
import turbolift.Signature
import turbolift.interpreter.Interpreter
import turbolift.internals.executor.Executor


private[turbolift] final class Env(
  val executor: Executor,
  val tickLow: Int,
  val tickHigh: Int,
  val currentWarp: WarpImpl | Null,
  val shadowMap: ShadowMap,
  val isParallelismRequested: Boolean = true,
  val isCancellable: Boolean,
):
  def copy(
    executor: Executor = executor,
    tickLow: Int = tickLow,
    tickHigh: Int = tickHigh,
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

  //@#@TODO temporary solution, until new layout of FiberImpl
  def fork: Env =
    val ok = (
      isCancellable &&
      (currentWarp == null) &&
      shadowMap.isEmpty
    )
    if ok then
      this
    else
      copy(
        isCancellable = true,
        currentWarp = null,
        shadowMap = ShadowMap.empty,
      )


  def par(x: Boolean): Env =
    if isParallelismRequested == x then
      this
    else
      copy(isParallelismRequested = x)

  override def toString = s"Env#%04X".format(hashCode & 0xFFFF)



private[turbolift] object Env:
  def initial(
    executor: Executor,
    tickLow: Int,
    tickHigh: Int,
  ): Env =
    new Env(
      executor = executor,
      tickHigh = tickHigh,
      tickLow = tickLow,
      currentWarp = null,
      shadowMap = ShadowMap.empty,
      isCancellable = true,
    )

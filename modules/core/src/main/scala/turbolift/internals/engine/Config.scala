package turbolift.internals.engine
import java.util.concurrent.Executor


private[internals] final class Config(
  val executor: Executor,
  val tickLow: Short,
  val tickHigh: Short,
  val isParallelismRequested: Boolean = true,
):
  def copy(
    executor: Executor = executor,
    tickLow: Short = tickLow,
    tickHigh: Short = tickHigh,
    isParallelismRequested: Boolean = isParallelismRequested,
  ) = new Config(
    executor = executor,
    tickLow = tickLow,
    tickHigh = tickHigh,
    isParallelismRequested = isParallelismRequested,
  )

  def par(x: Boolean): Config =
    if isParallelismRequested == x then
      this
    else
      copy(isParallelismRequested = x)


private[internals] object Config:
  def default(executor: Executor): Config =
    new Config(
      executor = executor,
      tickHigh = 10,
      tickLow = 500,
    )

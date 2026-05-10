package turbolift.runtime
import java.util.concurrent.{Executor => JExecutor}
import scala.concurrent.ExecutionContext
import turbolift.internals.executor.{Executor, ForeignExecutor}


final case class RuntimeConfig(
  executor: () => Executor,
  autoYieldThreshold: Int,
  cancellationCheckThreshold: Int,
):
  def withExecutor(executor: => Executor): RuntimeConfig = copy(executor = () => executor)


object RuntimeConfig:
  val default = RuntimeConfig(
    executor = () => Executor.multi,
    autoYieldThreshold = 1000,
    cancellationCheckThreshold = 20,
  )

  def apply(executor: => Executor): RuntimeConfig = default.withExecutor(executor)
  def fromJava(executor: => JExecutor): RuntimeConfig = default.withExecutor(ForeignExecutor.fromJava(executor))
  def fromScala(executor: => ExecutionContext): RuntimeConfig = default.withExecutor(ForeignExecutor.fromScala(executor))

  val MT = RuntimeConfig(Executor.multi)
  val ST = RuntimeConfig(Executor.zero())

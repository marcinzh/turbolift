package turbolift.runtime
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext
import turbolift.Computation
import turbolift.data.Outcome
import turbolift.effects.IO
import turbolift.internals.executor.{Executor, ForeignExecutor}
import turbolift.internals.engine.{Env, FiberImpl}


final class Runtime private (val initialEnv: Env):
  def runSync[A](comp: Computation[A, ?], name: String = ""): Outcome[A] =
    Runtime.register(this)
    executor.runSync(comp, this, name)

  def runAsync[A](comp: Computation[A, ?], callback: Outcome[A] => Unit, name: String = ""): Unit =
    Runtime.register(this)
    executor.runAsync(comp, this, callback, name)

  def executor: Executor = initialEnv.executor

  /** For interoperbility. */
  val asExecutionContext: ExecutionContext =
    executor match
      case executor: ForeignExecutor => executor.underlying
      case _ => new Runtime.ExecutionContextAdapter(this)


object Runtime:
  def apply(config: RuntimeConfig): Runtime =
    val initialEnv: Env = Env.initial(
      executor = config.executor.apply(),
      tickLow = config.autoYieldThreshold,
      tickHigh = config.cancellationCheckThreshold,
    )
    new Runtime(initialEnv)

  def apply(mode: Mode): Runtime =
    mode match
      case Some(config) => Runtime(config)
      case None => currentOrDefault()

  def default(): Runtime = Runtime(RuntimeConfig.default)

  private val currentVar = new AtomicReference[Runtime | Null]

  private def register(runtime: Runtime): Unit = currentVar.compareAndExchange(null, runtime)

  def currentOrDefault(): Runtime =
    currentVar.get match
      case runtime: Runtime => runtime
      case null =>
        val newRuntime = default()
        currentVar.compareAndExchange(null, newRuntime) match
          case null => newRuntime
          case oldRuntime: Runtime => oldRuntime

  /** For interoperbility. */
  def getExecutorFromExecutionContext(ec: ExecutionContext): Executor =
    ec match
      case ExecutionContextAdapter(runtime) => runtime.executor
      case _ => ForeignExecutor.fromScala(ec)

  /** For interoperbility. */
  private final case class ExecutionContextAdapter(runtime: Runtime) extends ExecutionContext:
    override def execute(runnable: Runnable): Unit = runtime.runSync(IO(runnable.run()))
    override def reportFailure(cause: Throwable): Unit = ()


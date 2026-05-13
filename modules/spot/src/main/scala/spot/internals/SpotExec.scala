package spot.internals
import scala.concurrent.ExecutionContext
import turbolift.!!
import turbolift.effects.IO
import turbolift.runtime.Runtime
import turbolift.internals.executor.Executor


object SpotExec:
  def current: ExecutionContext !! IO = !!.getRuntime.map(_.asExecutionContext)

  def evalOn[A, U <: IO](comp: A !! U, ec: ExecutionContext): A !! U =
    val exec = ec match
      case exec: Executor => exec
      case _ => Runtime.getExecutorFromExecutionContext(ec)
    comp.executeOn(exec)

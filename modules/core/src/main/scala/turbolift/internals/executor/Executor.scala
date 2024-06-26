package turbolift.internals.executor
import scala.concurrent.ExecutionContext
import turbolift.Computation
import turbolift.effects.IO
import turbolift.mode.Mode
import turbolift.io.Outcome
import turbolift.internals.engine.FiberImpl


private[internals] trait Resumer:
  def resume(fiber: FiberImpl): Unit
  def executor: Executor


trait Executor extends ExecutionContext with Resumer:
  final override def executor: Executor = this
  final override def execute(runnable: Runnable): Unit = runSync(IO(runnable.run()), "")
  final override def reportFailure(cause: Throwable): Unit = ()

  def runSync[A](comp: Computation[A, ?], name: String): Outcome[A]
  def runAsync[A](comp: Computation[A, ?], name: String, callback: Outcome[A] => Unit): Unit


object Executor:
  def RE: Executor = ReentrantExecutor.default
  def MT: Executor = ForeignExecutor.default
  def ST: Executor = new ZeroThreadedExecutor

  def pick(mode: Mode): Executor = pick(mode.multiThreaded)
  def pick(multiThreaded: Boolean): Executor = if multiThreaded then MT else ST

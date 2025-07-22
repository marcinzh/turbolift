package turbolift.internals.executor
import java.util.concurrent.{Executor => JExecutor}
import scala.concurrent.ExecutionContext
import turbolift.Computation
import turbolift.effects.IO
import turbolift.mode.Mode
import turbolift.data.Outcome
import turbolift.internals.engine.concurrent.FiberImpl


trait Executor extends ExecutionContext:
  final override def execute(runnable: Runnable): Unit = runSync(IO(runnable.run()), "")
  final override def reportFailure(cause: Throwable): Unit = ()

  private[turbolift] def resume(fiber: FiberImpl): Unit
  def runSync[A](comp: Computation[A, ?], name: String = ""): Outcome[A]
  def runAsync[A](comp: Computation[A, ?], name: String, callback: Outcome[A] => Unit): Unit


object Executor:
  def zero(): Executor = new ZeroThreadedExecutor
  def multi: Executor = ForeignExecutor.default
  def reentrant: Executor = ReentrantExecutor.default
  def fromScala(e: ExecutionContext): Executor = ForeignExecutor.fromScala(e)
  def fromJava(e: JExecutor): Executor = ForeignExecutor.fromJava(e)

  def MT: Executor = multi
  def ST: Executor = zero()

  def pick(mode: Mode): Executor = pick(mode.multiThreaded)
  def pick(multiThreaded: Boolean): Executor = if multiThreaded then MT else ST

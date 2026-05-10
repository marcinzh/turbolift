package turbolift.internals.executor
import java.util.concurrent.{Executor => JExecutor}
import scala.concurrent.ExecutionContext
import turbolift.Computation
import turbolift.data.Outcome
import turbolift.internals.engine.FiberImpl
import turbolift.runtime.Runtime


trait Executor:
  private[turbolift] def resume(fiber: FiberImpl): Unit
  def runSync[A](comp: Computation[A, ?], runtime: Runtime, name: String): Outcome[A]
  def runAsync[A](comp: Computation[A, ?], runtime: Runtime, callback: Outcome[A] => Unit, name: String): Unit


object Executor:
  def zero(): Executor = new ZeroThreadedExecutor
  def multi: Executor = ForeignExecutor.default
  def reentrant: Executor = ReentrantExecutor.default
  def fromScala(e: ExecutionContext): Executor = ForeignExecutor.fromScala(e)
  def fromJava(e: JExecutor): Executor = ForeignExecutor.fromJava(e)

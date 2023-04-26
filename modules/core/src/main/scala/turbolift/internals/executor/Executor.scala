package turbolift.internals.executor
import scala.util.Try
import turbolift.Computation
import turbolift.internals.engine.{Config, FiberImpl}


private[internals] trait Executor:
  def enqueue(fiber: FiberImpl): Unit

  def start[A](comp: Computation[?, ?], config: Config): FiberImpl

  final def run[A](comp: Computation[A, ?], config: Config): Try[A] =
    start(comp, config).toTry()

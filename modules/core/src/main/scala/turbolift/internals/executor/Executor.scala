package turbolift.internals.executor
import scala.util.Try
import turbolift.Computation
import turbolift.effects.IO
import turbolift.mode.Mode
import turbolift.internals.engine.{Config, FiberImpl, AnyCallback}


private[internals] trait Executor:
  def start(fiber: FiberImpl): Unit
  def enqueue(fiber: FiberImpl): Unit

  final def runSync[A](comp: Computation[A, ?]): Try[A] =
    val config = Config.default(executor = this)
    val fiber = new FiberImpl(comp, config)
    start(fiber)
    fiber.unsafeAwait()

  final def runAsync[A](comp: Computation[A, IO], callback: Try[A] => Unit): Unit =
    val config = Config.default(executor = this)
    val fiber = new FiberImpl(comp, config, callback.asInstanceOf[AnyCallback])
    start(fiber)


object Executor:
  def MT: Executor = MultiThreadedExecutor.default
  def ST: Executor = new ZeroThreadedExecutor

  def pick(mode: Mode): Executor = pick(mode.multiThreaded)
  def pick(multiThreaded: Boolean): Executor = if multiThreaded then MT else ST

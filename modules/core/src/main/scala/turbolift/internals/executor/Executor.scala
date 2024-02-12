package turbolift.internals.executor
import turbolift.Computation
import turbolift.effects.IO
import turbolift.mode.Mode
import turbolift.io.Outcome
import turbolift.internals.engine.FiberImpl


private[internals] trait Executor:
  def start(fiber: FiberImpl): Unit
  def resume(fiber: FiberImpl): Unit
  def detectReentry(): Boolean

  final def runSync[A](comp: Computation[A, ?]): Outcome[A] =
    val fiber = FiberImpl.create(comp, this)
    start(fiber)
    fiber.unsafeAwait()

  final def runAsync[A](comp: Computation[A, IO], callback: Outcome[A] => Unit): Unit =
    val fiber = FiberImpl.create(comp, this, callback)
    start(fiber)


object Executor:
  def MT: Executor = MultiThreadedExecutor.default
  def ST: Executor = new ZeroThreadedExecutor

  def pick(mode: Mode): Executor = pick(mode.multiThreaded)
  def pick(multiThreaded: Boolean): Executor = if multiThreaded then MT else ST

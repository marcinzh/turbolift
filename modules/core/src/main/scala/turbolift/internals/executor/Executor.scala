package turbolift.internals.executor
import turbolift.Computation
import turbolift.effects.IO
import turbolift.mode.Mode
import turbolift.io.Outcome
import turbolift.internals.engine.FiberImpl


private[internals] trait Executor:
  def start(fiber: FiberImpl, isReentry: Boolean): Unit
  def resume(fiber: FiberImpl): Unit
  protected def detectReentry(): Boolean

  final def runSync[A](comp: Computation[A, ?]): Outcome[A] =
    val isReentry = detectReentry()
    val fiber = FiberImpl.create(comp, this, isReentry)
    start(fiber, isReentry)
    fiber.unsafeAwait()

  final def runAsync[A](comp: Computation[A, IO], callback: Outcome[A] => Unit): Unit =
    val isReentry = detectReentry()
    val fiber = FiberImpl.create(comp, this, isReentry, callback)
    start(fiber, isReentry)


object Executor:
  def MT: Executor = MultiThreadedExecutor.default
  def ST: Executor = new ZeroThreadedExecutor

  def pick(mode: Mode): Executor = pick(mode.multiThreaded)
  def pick(multiThreaded: Boolean): Executor = if multiThreaded then MT else ST

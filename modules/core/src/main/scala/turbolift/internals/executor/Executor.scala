package turbolift.internals.executor
import turbolift.Computation
import turbolift.effects.IO
import turbolift.mode.Mode
import turbolift.io.Outcome
import turbolift.internals.engine.{Env, FiberImpl, AnyCallback}


private[internals] trait Executor:
  def start(fiber: FiberImpl): Unit
  def enqueue(fiber: FiberImpl): Unit

  final def runSync[A](comp: Computation[A, ?]): Outcome[A] =
    val env = Env.default(executor = this)
    val fiber = new FiberImpl(comp, env)
    start(fiber)
    fiber.unsafeAwait()

  final def runAsync[A](comp: Computation[A, IO], callback: Outcome[A] => Unit): Unit =
    val env = Env.default(executor = this)
    val fiber = new FiberImpl(comp, env, callback.asInstanceOf[AnyCallback])
    start(fiber)


object Executor:
  def MT: Executor = MultiThreadedExecutor.default
  def ST: Executor = new ZeroThreadedExecutor

  def pick(mode: Mode): Executor = pick(mode.multiThreaded)
  def pick(multiThreaded: Boolean): Executor = if multiThreaded then MT else ST

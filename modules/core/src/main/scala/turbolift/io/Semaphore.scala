package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.util.SemaphoreImpl


sealed trait Semaphore:
  final def acquire(count: Long): Unit !! IO = CC.intrinsic(_.intrinsicAcquireSemaphore(this, count))

  final def tryAcquire(count: Long): Boolean !! IO = !!.impure(unsafeTryAcquire(count))

  final def release(count: Long): Unit !! IO = !!.impure(unsafeRelease(count))

  final def use[A, U <: IO](count: Long)(body: A !! U): A !! U = IO.bracket(acquire(count))(_ => release(count))(_ => body)

  final def tryUse[A, U <: IO](count: Long)(body: A !! U): Option[A] !! U =
    IO.bracket(tryAcquire(count))(ok => !!.when(ok)(release(count)))(ok => if ok then body.map(Some(_)) else !!.none)

  def unsafeRelease(count: Long): Unit
  def unsafeTryAcquire(count: Long): Boolean

  private[turbolift] final def asImpl: SemaphoreImpl = asInstanceOf[SemaphoreImpl]


object Semaphore:
  private[turbolift] trait Unsealed extends Semaphore

  def apply(initial: Int): Semaphore !! IO = create(initial)
  def create(initial: Int): Semaphore !! IO = !!.impure(unsafeCreate(initial))
  def unsafeCreate(initial: Int): Semaphore = new SemaphoreImpl(initial)

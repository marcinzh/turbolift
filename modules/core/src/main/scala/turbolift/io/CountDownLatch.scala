package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.{FiberImpl, Halt}
import turbolift.internals.engine.concurrent.CountDownLatchImpl


sealed trait CountDownLatch:
  private[turbolift] def intrinsicAwait(waiter: FiberImpl): Halt

  final def await: Unit !! IO = CC.intrinsic(intrinsicAwait(_))

  final def release: Unit !! IO = !!.impure(unsafeRelease())

  def unsafeRelease(): Unit

  private[turbolift] final def asImpl: CountDownLatchImpl = asInstanceOf[CountDownLatchImpl]


object CountDownLatch:
  private[turbolift] trait Unsealed extends CountDownLatch

  def apply(initial: Int): CountDownLatch !! IO = create(initial)
  def create(initial: Int): CountDownLatch !! IO = !!.impure(unsafeCreate(initial))
  def unsafeCreate(initial: Int): CountDownLatch = new CountDownLatchImpl(initial)

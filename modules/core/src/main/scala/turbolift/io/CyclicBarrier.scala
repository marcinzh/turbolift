package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.{FiberImpl, Halt}
import turbolift.internals.engine.concurrent.CyclicBarrierImpl


sealed trait CyclicBarrier:
  private[turbolift] def intrinsicAwait(waiter: FiberImpl): Halt

  final def await: Unit !! IO = CC.intrinsic(intrinsicAwait(_))

  private[turbolift] final def asImpl: CyclicBarrierImpl = asInstanceOf[CyclicBarrierImpl]


object CyclicBarrier:
  private[turbolift] trait Unsealed extends CyclicBarrier

  def apply(capacity: Int): CyclicBarrier !! IO = create(capacity)
  def create(capacity: Int): CyclicBarrier !! IO = !!.impure(unsafeCreate(capacity))
  def unsafeCreate(capacity: Int): CyclicBarrier = new CyclicBarrierImpl(capacity)

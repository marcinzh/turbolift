package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.util.CyclicBarrierImpl


sealed trait CyclicBarrier:
  final def await: Unit !! IO = CC.intrinsic(_.intrinsicAwaitCyclicBarrier(this))

  private[turbolift] final def asImpl: CyclicBarrierImpl = asInstanceOf[CyclicBarrierImpl]


object CyclicBarrier:
  private[turbolift] trait Unsealed extends CyclicBarrier

  def apply(capacity: Int): CyclicBarrier !! IO = create(capacity)
  def create(capacity: Int): CyclicBarrier !! IO = !!.impure(new CyclicBarrierImpl(capacity))

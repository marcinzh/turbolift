package turbolift.io
import turbolift.{!!, ComputationCases => CC}
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.util.CountDownLatchImpl


sealed trait CountDownLatch:
  final def await: Unit !! IO = CC.intristic(_.intristicAwaitCountDownLatch(this))

  final def release: Unit !! IO = !!.impure(unsafeRelease())

  def unsafeRelease(): Unit

  private[turbolift] final def asImpl: CountDownLatchImpl = asInstanceOf[CountDownLatchImpl]


object CountDownLatch:
  private[turbolift] trait Unsealed extends CountDownLatch

  def fresh(initial: Int): CountDownLatch !! IO = !!.impure(new CountDownLatchImpl(initial))

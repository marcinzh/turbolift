package turbolift.internals.engine.concurrent.util
import turbolift.!!
import turbolift.io.{EffectfulVar, Zipper}
import turbolift.effects.Broken
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}
import turbolift.internals.engine.Misc.AnyComp


private[turbolift] final class EffectfulVarImpl extends Waitee with EffectfulVar.Unsealed:
  @volatile var isReady: Boolean = false //// Enables fast path
  private var theFirstShot: AnyComp | Null = null
  private var theNextShot: AnyComp | Null = null


  def getNextShot: AnyComp = theNextShot.nn


  def tryGetAwaitedBy(waiter: FiberImpl, isWaiterCancellable: Boolean): (Int, AnyComp | Null) =
    var waiterWillRun: AnyComp | Null = null

    val waiterCode =
      atomicallyBoth(waiter, isWaiterCancellable) {
        if isPending then
          subscribeWaiterUnsync(waiter)
          Bits.WaiterSubscribed
        else
          if theFirstShot != null then
            waiterWillRun = theFirstShot
            theFirstShot = null
            isReady = true
          else
            waiterWillRun = theNextShot
          Bits.WaiteeAlreadyCompleted
      }

    (waiterCode, waiterWillRun)


  override def unsafeTryPut(zipper: Zipper.Untyped): Boolean =
    val firstShot: AnyComp = zipper.run.map(Some(_))
    val nextShot: AnyComp = !!.pure(zipper.toOption)

    val willFinalize =
      atomically {
        if isPending then
          setCompletionToSuccess()
          theNextShot = nextShot
          val x = firstWaiter
          if x == null then
            theFirstShot = firstShot
          else
            x.standbyWaiterComp(firstShot)
            isReady = true
          true
        else
          false
      }

    if willFinalize then
      finallyNotifyAllWaiters()
    willFinalize

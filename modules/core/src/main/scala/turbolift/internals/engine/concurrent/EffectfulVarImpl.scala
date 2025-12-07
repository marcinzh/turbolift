package turbolift.internals.engine.concurrent
import turbolift.!!
import turbolift.io.{EffectfulVar, Zipper}
import turbolift.internals.engine.{Waitee, FiberImpl, Halt, Tag}
import turbolift.internals.engine.Misc.AnyComp


private[turbolift] final class EffectfulVarImpl extends Waitee with EffectfulVar.Unsealed:
  @volatile var isReady: Boolean = false //// Enables fast path
  private var theFirstShot: AnyComp | Null = null
  private var theNextShot: AnyComp | Null = null


  //// only used when handling Tag.NotifyEffectfulVar
  def getNextShot: AnyComp = theNextShot.nn


  override def intrinsicGetOption(waiter: FiberImpl): Halt =
    if isReady then
      waiter.willContinueEff(theNextShot.nn)
      Halt.Continue
    else
      waiter.willContinueTag(Tag.NotifyEffectfulVar, this)
      var savedComp: AnyComp | Null = null

      val halt =
        atomicallyBoth(waiter) {
          if isPending then
            subscribeWaiterUnsync(waiter)
            Halt.Retire
          else
            if theFirstShot != null then
              savedComp = theFirstShot
              theFirstShot = null
              isReady = true
            else
              savedComp = theNextShot
            Halt.Continue
        }

      if savedComp != null then
        waiter.willContinueEff(savedComp.nn)
      halt


  override def unsafeTryPut(zipper: Zipper.Untyped): Boolean =
    val firstShot: AnyComp = zipper.run.map(Some(_))
    val nextShot: AnyComp = !!.pure(zipper.toOption)

    val willFinalize =
      atomically {
        if isPending then
          setCompletion()
          theNextShot = nextShot
          val x = theFirstWaiter
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

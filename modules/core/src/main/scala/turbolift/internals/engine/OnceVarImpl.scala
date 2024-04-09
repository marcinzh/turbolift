package turbolift.internals.engine
import turbolift.io.OnceVar
import OnceVarImpl.Empty


private[turbolift] final class OnceVarImpl extends Waitee with OnceVar.Unsealed:
  @volatile var theContent: Any = Empty


  def tryGetAwaitedBy(waiter: FiberImpl): Int =
    waiter.synchronized {
      if waiter.isCancellationUnlatched then
        waiter.setCancellationLatch()
        Bits.WaiterAlreadyCancelled
      else
        synchronized {
          if isPending then
            subscribeWaiterUnsync(waiter)
            Bits.WaiterSubscribed
          else
            Bits.WaiteeAlreadyCompleted
        }
    }


  override def unsafeTryPut(value: Any): Boolean =
    val willFinalize =
      synchronized {
        if isPending then
          varyingBits = (varyingBits | Bits.Completion_Success).toByte
          theContent = value
          true
        else
          false
      }

    if willFinalize then
      notifyAllWaiters()
    willFinalize


  override def unsafeTryGet: Option[Any] =
    val x = theContent
    if Empty != x then
      Some(x)
    else
      None


private object OnceVarImpl:
  object Empty

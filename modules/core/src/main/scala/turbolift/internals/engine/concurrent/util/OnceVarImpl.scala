package turbolift.internals.engine.concurrent.util
import turbolift.io.OnceVar
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}
import OnceVarImpl.Empty


private[turbolift] final class OnceVarImpl extends Waitee with OnceVar.Unsealed:
  @volatile var theContent: Any = Empty


  def tryGetAwaitedBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    atomicallyBoth(waiter, isWaiterCancellable) {
      if isPending then
        subscribeWaiterUnsync(waiter)
        Bits.WaiterSubscribed
      else
        Bits.WaiteeAlreadyCompleted
    }


  override def unsafeTryPut(value: Any): Boolean =
    val willFinalize =
      atomically {
        if isPending then
          setCompletionToSuccess()
          theContent = value
          true
        else
          false
      }

    if willFinalize then
      finallyNotifyAllWaiters()
    willFinalize


  override def unsafeTryGet: Option[Any] =
    val x = theContent
    if Empty != x then
      Some(x)
    else
      None


private[engine] object OnceVarImpl:
  object Empty

package turbolift.internals.engine.concurrent
import turbolift.data.Exceptions
import turbolift.io.OnceVar
import turbolift.internals.engine.{Waitee, FiberImpl, Halt, Tag}
import OnceVarImpl.Empty


private[turbolift] final class OnceVarImpl extends Waitee with OnceVar.Unsealed with Function0[Any]:
  @volatile var theContent: Any = Empty

  override def unsafeAsThunk: () => Any = this

  override def apply(): Any =
    val x = theContent
    if Empty != theContent then x else throw new Exceptions.TieTheKnot


  override def intrinsicGet(waiter: FiberImpl): Halt =
    val value = theContent
    if Empty != value then
      waiter.willContinuePure(value)
      Halt.Continue
    else
      waiter.willContinueTag(Tag.NotifyOnceVar, this)

      val halt =
        atomicallyBoth(waiter) {
          if isPending then
            subscribeWaiterUnsync(waiter)
            Halt.Retire
          else
            Halt.Continue
        }

      if halt == Halt.Continue then
        waiter.willContinuePure(theContent)
      halt


  override def unsafeTryPut(value: Any): Boolean =
    val willFinalize =
      atomically {
        if isPending then
          setCompletion()
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

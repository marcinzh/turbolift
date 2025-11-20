package turbolift.internals.engine.concurrent
import turbolift.effects.IO
import turbolift.io.Channel
import turbolift.internals.engine.{Waitee, FiberImpl, Halt}
import ChannelImpl.{Buffer, Element, asElement, OVERFLOW, UNDERFLOW}


private[turbolift] final class ChannelImpl(val currCapacity: Int) extends Waitee with Channel.Unsealed:
  private var currSize: Int = 0
  private var currHead: Buffer | Null = null
  private var kindOfWaiters: Int = 0 //// meaningless when waiter list is empty


  override def intrinsicGet(waiter: FiberImpl): Halt =
    waiter.willContinuePure(null)
    var waiterToResume: FiberImpl | Null = null

    val halt =
      atomicallyBoth(waiter) {
        val x = theFirstWaiter
        if x == null then
          if currSize == 0 then
            kindOfWaiters = UNDERFLOW
            subscribeFirstWaiterUnsync(waiter)
            Halt.Retire
          else
            currSize -= 1
            waiter.willContinuePure(removeFirst())
            Halt.Continue
        else
          if kindOfWaiters == OVERFLOW then
            waiter.willContinuePure(insertLastAndRemoveFirst(x.takeWaiterState().asElement))
            waiterToResume = x
            removeFirstWaiter()
            x.standbyWaiter()
            Halt.Continue
          else
            subscribeWaiterUnsync(waiter)
            Halt.Retire
      }

    if waiterToResume != null then
      waiterToResume.nn.resume()

    halt


  //// Same as `intrinsicGet` but without subscribing the waiter
  override def unsafeTryGet(): Option[Any] =
    var savedValue: Any = null
    var waiterToResume: FiberImpl | Null = null

    val ok =
      atomically {
        val x = theFirstWaiter
        if x == null then
          if currSize == 0 then
            false
          else
            currSize -= 1
            savedValue = removeFirst()
            true
        else
          if kindOfWaiters == OVERFLOW then
            savedValue = insertLastAndRemoveFirst(x.takeWaiterState().asElement)
            waiterToResume = x
            removeFirstWaiter()
            x.standbyWaiter()
            true
          else
            false
      }

    if waiterToResume != null then
      waiterToResume.nn.resume()

    if ok then Some(savedValue) else None


  override def intrinsicPut(waiter: FiberImpl, value: Any): Halt =
    waiter.willContinuePure(())
    var waiterToResume: FiberImpl | Null = null

    val halt =
      atomicallyBoth(waiter) {
        val x = theFirstWaiter
        if x == null then
          if currSize == currCapacity then
            kindOfWaiters = OVERFLOW
            waiter.theWaiterStateAny = value
            subscribeFirstWaiterUnsync(waiter)
            Halt.Retire
          else
            currSize += 1
            insertLast(value.asElement)
            Halt.Continue
        else
          if kindOfWaiters == UNDERFLOW then
            waiterToResume = x
            removeFirstWaiter()
            x.standbyWaiterPure(value)
            Halt.Continue
          else
            waiter.theWaiterStateAny = value
            subscribeWaiterUnsync(waiter)
            Halt.Retire
      }

    if waiterToResume != null then
      waiterToResume.nn.resume()

    halt


  //// Same as `intrinsicPut` but without subscribing the waiter
  override def unsafeTryPut(value: Any): Boolean =
    var waiterToResume: FiberImpl | Null = null

    val ok =
      atomically {
        val x = theFirstWaiter
        if x == null then
          if currSize == currCapacity then
            false
          else
            currSize += 1
            insertLast(value.asElement)
            true
        else
          if kindOfWaiters == UNDERFLOW then
            waiterToResume = x
            removeFirstWaiter()
            x.standbyWaiterPure(value)
            true
          else
            false
      }

    if waiterToResume != null then
      waiterToResume.nn.resume()

    ok


  override def unsafeStatus(): Channel.Status =
    var savedSize: Int = 0
    var savedCapacity: Int = 0
    var savedIsBlocking: Boolean = false

    atomically {
      savedSize = currSize
      savedCapacity = currCapacity
      savedIsBlocking = theFirstWaiter != null
    }

    Channel.Status(
      size = savedSize,
      capacity = savedCapacity,
      isBlocking = savedIsBlocking,
    )


  //-------------------------------------------------------------------
  // Aux
  //-------------------------------------------------------------------


  final def insertLastAndRemoveFirst(newValue: Element): Element =
    val first = currHead
    if first != null then
      val oldValue = first.value
      first.value = newValue
      currHead = first.next
      oldValue
    else
      newValue


  final def insertLast(value: Element): Unit =
    val first = currHead
    val newLast = new Buffer(value)
    if first == null then
      newLast.next = newLast
      newLast.prev = newLast
      currHead = newLast
    else
      val oldLast = first.prev
      newLast.next = first
      first.prev = newLast
      oldLast.next = newLast
      newLast.prev = oldLast


  final def removeFirst(): Element =
    val first = currHead.nn
    val last = first.prev
    val second = first.next
    if first == second then
      currHead = null
    else
      last.next = second
      second.prev = last
      currHead = second
    first.value


private[turbolift] object ChannelImpl:
  inline val OVERFLOW = 0
  inline val UNDERFLOW = 1


  final class Buffer(var value: Element):
    var prev: Buffer = null.asInstanceOf[Buffer]
    var next: Buffer = null.asInstanceOf[Buffer]


  opaque type Element = Any

  object Element:
    extension (x: Element) inline def unwrap: Any = x

  extension (x: Any) inline def asElement: Element = x

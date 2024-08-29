package turbolift.internals.engine.concurrent.util
import turbolift.io.Channel
import turbolift.internals.engine.concurrent.{Bits, Waitee, FiberImpl}
import ChannelImpl.{Buffer, Element, asElement, OVERFLOW, UNDERFLOW}


private[turbolift] final class ChannelImpl(val currCapacity: Int) extends Waitee with Channel.Unsealed:
  private var currSize: Int = 0
  private var currHead: Buffer | Null = null
  private var kindOfWaiters: Int = 0 //// meaningless when waiter list is empty


  def tryGetBy(waiter: FiberImpl, isWaiterCancellable: Boolean): (Int, Any) =
    var savedValue: Any = null
    var savedWaiter: FiberImpl | Null = null

    val result =
      atomicallyBoth(waiter, isWaiterCancellable) {
        val x = firstWaiter
        if x == null then
          if currSize == 0 then
            kindOfWaiters = UNDERFLOW
            subscribeFirstWaiterUnsync(waiter)
            Bits.WaiterSubscribed
          else
            currSize -= 1
            savedValue = removeFirst()
            Bits.WaiteeAlreadyCompleted
        else
          if kindOfWaiters == OVERFLOW then
            savedValue = insertLastAndRemoveFirst(x.suspendedPayload.asElement)
            savedWaiter = x
            removeFirstWaiter()
            x.standbyWaiter(())
            Bits.WaiteeAlreadyCompleted
          else
            subscribeWaiterUnsync(waiter)
            Bits.WaiterSubscribed
      }

    if savedWaiter != null then
      savedWaiter.nn.resume()
    (result, savedValue)


  //// Same as `tryGetBy` but without subscribing the waiter
  override def unsafeTryGet(): Option[Any] =
    var savedValue: Any = null
    var savedWaiter: FiberImpl | Null = null

    val ok =
      atomically {
        val x = firstWaiter
        if x == null then
          if currSize == 0 then
            false
          else
            currSize -= 1
            savedValue = removeFirst()
            true
        else
          if kindOfWaiters == OVERFLOW then
            savedValue = insertLastAndRemoveFirst(x.suspendedPayload.asElement)
            savedWaiter = x
            removeFirstWaiter()
            x.standbyWaiter(())
            true
          else
            false
      }

    if savedWaiter != null then
      savedWaiter.nn.resume()
    if ok then Some(savedValue) else None


  def tryPutBy(waiter: FiberImpl, isWaiterCancellable: Boolean): Int =
    var savedWaiter: FiberImpl | Null = null

    val result =
      atomicallyBoth(waiter, isWaiterCancellable) {
        val x = firstWaiter
        if x == null then
          if currSize == currCapacity then
            kindOfWaiters = OVERFLOW
            subscribeFirstWaiterUnsync(waiter)
            Bits.WaiterSubscribed
          else
            currSize += 1
            insertLast(waiter.suspendedPayload.asElement)
            Bits.WaiteeAlreadyCompleted
        else
          if kindOfWaiters == UNDERFLOW then
            savedWaiter = x
            removeFirstWaiter()
            x.standbyWaiter(waiter.suspendedPayload)
            Bits.WaiteeAlreadyCompleted
          else
            subscribeWaiterUnsync(waiter)
            Bits.WaiterSubscribed
      }

    if savedWaiter != null then
      savedWaiter.nn.resume()
    result


  //// Same as `tryPutBy` but without subscribing the waiter
  override def unsafeTryPut(value: Any): Boolean =
    var savedWaiter: FiberImpl | Null = null

    val ok =
      atomically {
        val x = firstWaiter
        if x == null then
          if currSize == currCapacity then
            false
          else
            currSize += 1
            insertLast(value.asElement)
            true
        else
          if kindOfWaiters == UNDERFLOW then
            savedWaiter = x
            removeFirstWaiter()
            x.standbyWaiter(value)
            true
          else
            false
      }

    if savedWaiter != null then
      savedWaiter.nn.resume()
    ok


  override def unsafeStatus(): Channel.Status =
    var savedSize: Int = 0
    var savedCapacity: Int = 0
    var savedIsBlocking: Boolean = false

    atomically {
      savedSize = currSize
      savedCapacity = currCapacity
      savedIsBlocking = firstWaiter != null
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

package turbolift.internals.engine.concurrent.atomic
import java.lang.invoke.{VarHandle, MethodHandles}


private[turbolift] class AtomicLongVH(protected var unsafeValue: Long):
  protected def getVH: Long = AtomicLongVH.theVH.get(this)
  
  protected def setVH(a: Long): Unit = AtomicLongVH.theVH.set(this, a)
  
  protected def gasVH(a: Long): Long = AtomicLongVH.theVH.getAndSet(this, a)

  protected def casVH(a: Long, b: Long): Boolean = AtomicLongVH.theVH.compareAndSet(this, a, b)

  protected def caxVH(a: Long, b: Long): Long = AtomicLongVH.theVH.compareAndExchange(this, a, b)


private[turbolift] object AtomicLongVH:
  private val theVH: VarHandle = MethodHandles
    .privateLookupIn(classOf[AtomicLongVH], MethodHandles.lookup()).nn
    .findVarHandle(classOf[AtomicLongVH], "unsafeValue", java.lang.Long.TYPE).nn

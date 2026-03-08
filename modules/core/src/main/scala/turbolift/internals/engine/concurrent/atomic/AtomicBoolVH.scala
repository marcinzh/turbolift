package turbolift.internals.engine.concurrent.atomic
import java.lang.invoke.{VarHandle, MethodHandles}


private[turbolift] class AtomicBoolVH(protected var unsafeValue: Boolean):
  protected def getVH: Boolean = AtomicBoolVH.theVH.get(this)
  
  protected def setVH(a: Boolean): Unit = AtomicBoolVH.theVH.set(this, a)
  
  protected def gasVH(a: Boolean): Boolean = AtomicBoolVH.theVH.getAndSet(this, a)

  protected def casVH(a: Boolean, b: Boolean): Boolean = AtomicBoolVH.theVH.compareAndSet(this, a, b)

  protected def caxVH(a: Boolean, b: Boolean): Boolean = AtomicBoolVH.theVH.compareAndExchange(this, a, b)


private[turbolift] object AtomicBoolVH:
  private val theVH: VarHandle = MethodHandles
    .privateLookupIn(classOf[AtomicBoolVH], MethodHandles.lookup()).nn
    .findVarHandle(classOf[AtomicBoolVH], "unsafeValue", java.lang.Boolean.TYPE).nn

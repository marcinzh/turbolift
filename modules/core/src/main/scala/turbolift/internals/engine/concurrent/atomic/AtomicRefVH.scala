package turbolift.internals.engine.concurrent.atomic
import java.lang.invoke.{VarHandle, MethodHandles}


private[turbolift] class AtomicRefVH[T](protected var unsafeValue: T):
  protected def getVH: T = AtomicRefVH.theVH.get(this)
  
  protected def setVH(a: T): Unit = AtomicRefVH.theVH.set(this, a)
  
  protected def gasVH(a: T): T = AtomicRefVH.theVH.getAndSet(this, a)

  protected def casVH(a: T, b: T): Boolean = AtomicRefVH.theVH.compareAndSet(this, a, b)

  protected def caxVH(a: T, b: T): T = AtomicRefVH.theVH.compareAndExchange(this, a, b)


private[turbolift] object AtomicRefVH:
  private val theVH: VarHandle = MethodHandles
    .privateLookupIn(classOf[AtomicRefVH[?]], MethodHandles.lookup()).nn
    .findVarHandle(classOf[AtomicRefVH[?]], "unsafeValue", classOf[Object]).nn

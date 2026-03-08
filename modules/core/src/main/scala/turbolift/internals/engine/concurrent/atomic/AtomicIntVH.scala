package turbolift.internals.engine.concurrent.atomic
import java.lang.invoke.{VarHandle, MethodHandles}


private[turbolift] class AtomicIntVH(protected var unsafeValue: Int):
  protected def getVH: Int = AtomicIntVH.theVH.get(this)
  
  protected def setVH(a: Int): Unit = AtomicIntVH.theVH.set(this, a)
  
  protected def gasVH(a: Int): Int = AtomicIntVH.theVH.getAndSet(this, a)

  protected def casVH(a: Int, b: Int): Boolean = AtomicIntVH.theVH.compareAndSet(this, a, b)

  protected def caxVH(a: Int, b: Int): Int = AtomicIntVH.theVH.compareAndExchange(this, a, b)


private[turbolift] object AtomicIntVH:
  private val theVH: VarHandle = MethodHandles
    .privateLookupIn(classOf[AtomicIntVH], MethodHandles.lookup()).nn
    .findVarHandle(classOf[AtomicIntVH], "unsafeValue", java.lang.Integer.TYPE).nn

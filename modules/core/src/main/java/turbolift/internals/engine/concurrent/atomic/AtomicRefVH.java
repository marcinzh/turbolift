package turbolift.internals.engine.concurrent.atomic;
import java.lang.invoke.VarHandle;
import java.lang.invoke.MethodHandles;


public class AtomicRefVH<T> {
  private T valueVH;
  private static final VarHandle nameVH;

  static {
    try {
      nameVH = MethodHandles.lookup().findVarHandle(AtomicRefVH.class, "valueVH", Object.class);
    } catch (ReflectiveOperationException e) {
      throw new ExceptionInInitializerError(e);
    }
  }

  public AtomicRefVH(T a) {
    valueVH = a;
  }

  protected T getVH() {
    return (T) nameVH.get(this);
  }

  protected void setVH(T a) {
    nameVH.set(this, a);
  }

  protected T gasVH(T a) {
    return (T) nameVH.getAndSet(this, a);
  }

  protected boolean casVH(T a, T b) {
    return nameVH.compareAndSet(this, a, b);
  }

  protected T caxVH(T a, T b) {
    return (T) nameVH.compareAndExchange(this, a, b);
  }
}

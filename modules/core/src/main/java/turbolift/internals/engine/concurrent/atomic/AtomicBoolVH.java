package turbolift.internals.engine.concurrent.atomic;
import java.lang.invoke.VarHandle;
import java.lang.invoke.MethodHandles;


public class AtomicBoolVH {
  private boolean valueVH;
  private static final VarHandle nameVH;

  static {
    try {
      nameVH = MethodHandles.lookup().findVarHandle(AtomicBoolVH.class, "valueVH", boolean.class);
    } catch (ReflectiveOperationException e) {
      throw new ExceptionInInitializerError(e);
    }
  }

  public AtomicBoolVH(boolean a) {
    valueVH = a;
  }

  protected boolean getVH() {
    return (boolean) nameVH.get(this);
  }

  protected void setVH(boolean a) {
    nameVH.set(this, a);
  }

  protected boolean gasVH(boolean a) {
    return (boolean) nameVH.getAndSet(this, a);
  }

  protected boolean casVH(boolean a, boolean b) {
    return nameVH.compareAndSet(this, a, b);
  }

  protected boolean caxVH(boolean a, boolean b) {
    return (boolean) nameVH.compareAndExchange(this, a, b);
  }
}

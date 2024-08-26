package turbolift.internals.engine.concurrent.atomic;
import java.lang.invoke.VarHandle;
import java.lang.invoke.MethodHandles;


public class AtomicIntVH {
  private int valueVH;
  private static final VarHandle nameVH;

  static {
    try {
      nameVH = MethodHandles.lookup().findVarHandle(AtomicIntVH.class, "valueVH", int.class);
    } catch (ReflectiveOperationException e) {
      throw new ExceptionInInitializerError(e);
    }
  }

  public AtomicIntVH(int a) {
    valueVH = a;
  }

  protected int getVH() {
    return (int) nameVH.get(this);
  }

  protected void setVH(int a) {
    nameVH.set(this, a);
  }

  protected int gasVH(int a) {
    return (int) nameVH.getAndSet(this, a);
  }

  protected boolean casVH(int a, int b) {
    return nameVH.compareAndSet(this, a, b);
  }

  protected int caxVH(int a, int b) {
    return (int) nameVH.compareAndExchange(this, a, b);
  }

  protected int gaaVH(int d) {
    return (int) nameVH.getAndAdd(this, d);
  }
}

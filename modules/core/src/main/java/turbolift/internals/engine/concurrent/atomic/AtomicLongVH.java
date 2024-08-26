package turbolift.internals.engine.concurrent.atomic;
import java.lang.invoke.VarHandle;
import java.lang.invoke.MethodHandles;


public class AtomicLongVH {
  private long valueVH;
  private static final VarHandle nameVH;

  static {
    try {
      nameVH = MethodHandles.lookup().findVarHandle(AtomicLongVH.class, "valueVH", long.class);
    } catch (ReflectiveOperationException e) {
      throw new ExceptionInInitializerError(e);
    }
  }

  public AtomicLongVH(long a) {
    valueVH = a;
  }

  protected long getVH() {
    return (long) nameVH.get(this);
  }

  protected void setVH(long a) {
    nameVH.set(this, a);
  }

  protected long gasVH(long a) {
    return (long) nameVH.getAndSet(this, a);
  }

  protected boolean casVH(long a, long b) {
    return nameVH.compareAndSet(this, a, b);
  }

  protected long caxVH(long a, long b) {
    return (long) nameVH.compareAndExchange(this, a, b);
  }

  protected long gaaVH(long d) {
    return (long) nameVH.getAndAdd(this, d);
  }
}

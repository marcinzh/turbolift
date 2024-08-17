package turbolift.internals.engine.concurrent;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;


public class SpinLock {
  private volatile boolean lockState = false;
  private static final VarHandle lockStateVH;

  static {
    try {
      lockStateVH = MethodHandles.lookup().findVarHandle(SpinLock.class, "lockState", boolean.class);
    } catch (ReflectiveOperationException e) {
      throw new ExceptionInInitializerError(e);
    }
  }

  public final boolean spinAcquire() {
    return lockStateVH.compareAndSet(this, false, true);
  }

  public final void spinRelease() {
    lockStateVH.set(this, false);
  }
}

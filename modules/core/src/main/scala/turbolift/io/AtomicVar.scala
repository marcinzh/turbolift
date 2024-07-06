package turbolift.io
import java.util.concurrent.atomic.{AtomicReference, AtomicInteger, AtomicLong, AtomicBoolean}
import scala.annotation.tailrec
import turbolift.!!
import turbolift.effects.IO


sealed trait AtomicVar[@specialized(Int, Long, Boolean) S] extends AtomicVar.Get[S] with AtomicVar.Put[S]:
  final def asGet: AtomicVar.Get[S] = this
  final def asPut: AtomicVar.Put[S] = this
  
  final def swap(s: S): S !! IO = !!.impure(unsafeSwap(s))
  final def modify(f: S => S): Unit !! IO = op(f, s => s, (_, _) => ())
  final def modifyGet(f: S => S): S !! IO = op(f, s => s, (_, s) => s)
  final def getModify(f: S => S): S !! IO = op(f, s => s, (s, _) => s)
  final def getModifyGet(f: S => S): (S, S) !! IO = op(f, s => s, (s1, s2) => (s1, s2))
  final def update[A](f: S => (A, S)): A !! IO = op(f, _._2, (_, as) => as._1)
  final def updateGet[A](f: S => (A, S)): (A, S) !! IO = op(f, _._2, (_, as) => as)
  final def getUpdate[A](f: S => (A, S)): (A, S) !! IO = op(f, _._2, (s, as) => (as._1, s))
  final def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! IO = op(f, _._2, (s, as) => (as._1, s, as._2))

  final def modifyEff[U <: IO](f: S => S !! U): Unit !! U = opEff(f, s => s, (_, _) => ())
  final def modifyGetEff[U <: IO](f: S => S !! U): S !! U = opEff(f, s => s, (_, s) => s)
  final def getModifyEff[U <: IO](f: S => S !! U): S !! U = opEff(f, s => s, (s, _) => s)
  final def getModifyGetEff[U <: IO](f: S => S !! U): (S, S) !! U = opEff(f, s => s, (s1, s2) => (s1, s2))
  final def updateEff[A, U <: IO](f: S => (A, S) !! U): A !! U = opEff(f, _._2, (_, as) => as._1)
  final def updateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U = opEff(f, _._2, (_, as) => as)
  final def getUpdateEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U = opEff(f, _._2, (s, as) => (as._1, s))
  final def getUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S, S) !! U = opEff(f, _._2, (s, as) => (as._1, s, as._2))

  final def trySwap(s: S): (S, Boolean) !! IO = tryOp(s => s, _ => s, (s, _, k) => (s, k))
  final def tryModify(f: S => S): Boolean !! IO = tryOp(f, s => s, (_, _, k) => k)
  final def tryModifyGet(f: S => S): (S, Boolean) !! IO = tryOp(f, s => s, (_, s, k) => (s, k))
  final def tryGetModify(f: S => S): (S, Boolean) !! IO = tryOp(f, s => s, (s, _, k) => (s, k))
  final def tryGetModifyGet(f: S => S): (S, S, Boolean) !! IO = tryOp(f, s => s, (s1, s2, k) => (s1, s2, k))
  final def tryUpdate[A](f: S => (A, S)): (A, Boolean) !! IO = tryOp(f, _._2, (_, as, k) => (as._1, k))
  final def tryUpdateGet[A](f: S => (A, S)): (A, S, Boolean) !! IO = tryOp(f, _._2, (_, as, k) => (as._1, as._2, k))
  final def tryGetUpdate[A](f: S => (A, S)): (A, S, Boolean) !! IO = tryOp(f, _._2, (s, as, k) => (as._1, s, k))
  final def tryGetUpdateGet[A](f: S => (A, S)): (A, S, S, Boolean) !! IO = tryOp(f, _._2, (s, as, k) => (as._1, s, as._2, k))

  def unsafeSwap(s: S): S
  def unsafeCompareAndSet(a: S, b: S): Boolean


  private final inline def op[A, B](inline f: S => A, inline g: A => S, inline h: (S, A) => B): B !! IO =
    @tailrec def loop: B =
      val s = unsafeGet
      val a = f(s)
      val s2 = g(a)
      if unsafeCompareAndSet(s, s2) then
        h(s, a)
      else
        loop
    !!.impure(loop)


  private final inline def opEff[A, B, U <: IO](inline f: S => A !! U, inline g: A => S, inline h: (S, A) => B): B !! U =
    def loop: B !! U =
      for
        s <- !!.impure(unsafeGet)
        a <- f(s)
        s2 = g(a)
        b <-
          if unsafeCompareAndSet(s, s2) then
            !!.pure(h(s, a))
          else
            !!.impureEff(loop)
      yield b
    loop


  private final inline def tryOp[A, B](inline f: S => A, inline g: A => S, inline h: (S, A, Boolean) => B): B !! IO =
    !!.impure:
      val s = unsafeGet
      val a = f(s)
      val s2 = g(a)
      val ok = unsafeCompareAndSet(s, s2)
      h(s, a, ok)


object AtomicVar:
  sealed trait Get[@specialized(Int, Long, Boolean) S]:
    final def get: S !! IO = !!.impure(unsafeGet)
    final def gets[A](f: S => A): A !! IO = !!.impure(f(unsafeGet))
    def unsafeGet: S


  sealed trait Put[@specialized(Int, Long, Boolean) S]:
    final def put(s: S): Unit !! IO = !!.impure(unsafePut(s))
    def unsafePut(s: S): Unit


  def fresh(initial: Int): AtomicVar[Int] !! IO = freshInt(initial)
  def fresh(initial: Long): AtomicVar[Long] !! IO = freshLong(initial)
  def fresh(initial: Boolean): AtomicVar[Boolean] !! IO = freshBoolean(initial)
  def fresh[S](initial: S): AtomicVar[S] !! IO = freshRef(initial)


  def freshInt(initial: Int): AtomicVar[Int] !! IO =
    !!.impure:
      val underlying = new AtomicInteger(initial)
      new AtomicVar[Int]:
        override def unsafeGet: Int = underlying.get
        override def unsafePut(s: Int): Unit = underlying.set(s)
        override def unsafeSwap(s: Int): Int = underlying.getAndSet(s)
        override def unsafeCompareAndSet(a: Int, b: Int): Boolean = underlying.compareAndSet(a, b)


  def freshLong(initial: Long): AtomicVar[Long] !! IO =
    !!.impure:
      val underlying = new AtomicLong(initial)
      new AtomicVar[Long]:
        override def unsafeGet: Long = underlying.get
        override def unsafePut(s: Long): Unit = underlying.set(s)
        override def unsafeSwap(s: Long): Long = underlying.getAndSet(s)
        override def unsafeCompareAndSet(a: Long, b: Long): Boolean = underlying.compareAndSet(a, b)


  def freshBoolean(initial: Boolean): AtomicVar[Boolean] !! IO =
    !!.impure:
      val underlying = new AtomicBoolean(initial)
      new AtomicVar[Boolean]:
        override def unsafeGet: Boolean = underlying.get
        override def unsafePut(s: Boolean): Unit = underlying.set(s)
        override def unsafeSwap(s: Boolean): Boolean = underlying.getAndSet(s)
        override def unsafeCompareAndSet(a: Boolean, b: Boolean): Boolean = underlying.compareAndSet(a, b)


  def freshRef[S](initial: S): AtomicVar[S] !! IO =
    !!.impure:
      val underlying = new AtomicReference(initial)
      new AtomicVar[S]:
        override def unsafeGet: S = underlying.get.asInstanceOf[S]
        override def unsafePut(s: S): Unit = underlying.set(s)
        override def unsafeSwap(s: S): S = underlying.getAndSet(s).asInstanceOf[S]
        override def unsafeCompareAndSet(a: S, b: S): Boolean = underlying.compareAndSet(a, b)

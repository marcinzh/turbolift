package turbolift.io
import scala.annotation.tailrec
import turbolift.!!
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.atomic.{AtomicRefVH, AtomicIntVH, AtomicLongVH, AtomicBoolVH}


sealed trait AtomicVar[@specialized(Int, Long, Boolean) S] extends AtomicVar.Get[S] with AtomicVar.Put[S]:
  final def asGet: AtomicVar.Get[S] = this
  final def asPut: AtomicVar.Put[S] = this

  def swap(s: S): S !! IO
  def modify(f: S => S): Unit !! IO
  def modifyGet(f: S => S): S !! IO
  def getModify(f: S => S): S !! IO
  def getModifyGet(f: S => S): (S, S) !! IO
  def update[A](f: S => (A, S)): A !! IO
  def updateGet[A](f: S => (A, S)): (A, S) !! IO
  def getUpdate[A](f: S => (A, S)): (A, S) !! IO
  def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! IO

  def swapEff[U <: IO](s: S !! U): S !! U
  def modifyEff[U <: IO](f: S => S !! U): Unit !! U
  def modifyGetEff[U <: IO](f: S => S !! U): S !! U
  def getModifyEff[U <: IO](f: S => S !! U): S !! U
  def getModifyGetEff[U <: IO](f: S => S !! U): (S, S) !! U
  def updateEff[A, U <: IO](f: S => (A, S) !! U): A !! U
  def updateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U
  def getUpdateEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U
  def getUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S, S) !! U

  def trySwap(s: S): Option[S] !! IO
  def tryModify(f: S => S): Boolean !! IO
  def tryModifyGet(f: S => S): Option[S] !! IO
  def tryGetModify(f: S => S): Option[S] !! IO
  def tryGetModifyGet(f: S => S): Option[(S, S)] !! IO
  def tryUpdate[A](f: S => (A, S)): Option[A] !! IO
  def tryUpdateGet[A](f: S => (A, S)): Option[(A, S)] !! IO
  def tryGetUpdate[A](f: S => (A, S)): Option[(A, S)] !! IO
  def tryGetUpdateGet[A](f: S => (A, S)): Option[(A, S, S)] !! IO

  def trySwapEff[U <: IO](s: S !! U): Option[S] !! U
  def tryModifyEff[U <: IO](f: S => S !! U): Boolean !! U
  def tryModifyGetEff[U <: IO](f: S => S !! U): Option[S] !! U
  def tryGetModifyEff[U <: IO](f: S => S !! U): Option[S] !! U
  def tryGetModifyGetEff[U <: IO](f: S => S !! U): Option[(S, S)] !! U
  def tryUpdateEff[A, U <: IO](f: S => (A, S) !! U): Option[A] !! U
  def tryUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): Option[(A, S)] !! U
  def tryGetUpdateEff[A, U <: IO](f: S => (A, S) !! U): Option[(A, S)] !! U
  def tryGetUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): Option[(A, S, S)] !! U

  def isLocked: Boolean !! IO
  def unsafeIsLocked(): Boolean


object AtomicVar:
  sealed trait Get[S]:
    final def get: S !! IO = !!.impure(unsafeGet)
    final def gets[A](f: S => A): A !! IO = !!.impure(f(unsafeGet))
    final def getsEff[A, U <: IO](f: S => A !! U): A !! U = !!.impure(unsafeGet).flatMap(f)
    def unsafeGet: S


  sealed trait Put[S]:
    final def put(s: S): Unit !! IO = !!.impure(unsafePut(s))
    final def putEff[U <: IO](comp: S !! U): Unit !! U = comp.map(unsafePut)
    def unsafePut(s: S): Unit


  def apply[S](initial: S): AtomicVar[S] !! IO = createLockless(initial)
  def createLockless[S](initial: S): AtomicVar[S] !! IO = !!.impure(unsafeCreateLocklessRef(initial))
  def createLockless(initial: Int): AtomicVar[Int] !! IO = !!.impure(unsafeCreateLocklessInt(initial))
  def createLockless(initial: Long): AtomicVar[Long] !! IO = !!.impure(unsafeCreateLocklessLong(initial))
  def createLockless(initial: Boolean): AtomicVar[Boolean] !! IO = !!.impure(unsafeCreateLocklessBoolean(initial))
  def createLockful[S](initial: S): AtomicVar[S] !! IO = !!.impure(unsafeCreateLockful(initial))
  def createLockful(initial: Int): AtomicVar[Int] !! IO = !!.impure(unsafeCreateLockful(initial))
  def createLockful(initial: Long): AtomicVar[Long] !! IO = !!.impure(unsafeCreateLockful(initial))
  def createLockful(initial: Boolean): AtomicVar[Boolean] !! IO = !!.impure(unsafeCreateLockful(initial))

  def unsafeCreateLockful[@specialized(Int, Long, Boolean) S](initial: S): Lockful[S] = new Lockful(initial)

  def unsafeCreateLocklessInt(initial: Int): AtomicVar[Int] =
    new AtomicIntVH(initial) with Lockless[Int]:
      override def unsafeGet: Int = getVH
      override def unsafePut(s: Int): Unit = setVH(s)
      override def unsafeSwap(s: Int): Int = gasVH(s)
      override def unsafeCompareAndSet(a: Int, b: Int): Boolean = casVH(a, b)


  def unsafeCreateLocklessLong(initial: Long): AtomicVar[Long] =
    new AtomicLongVH(initial) with Lockless[Long]:
      override def unsafeGet: Long = getVH
      override def unsafePut(s: Long): Unit = setVH(s)
      override def unsafeSwap(s: Long): Long = gasVH(s)
      override def unsafeCompareAndSet(a: Long, b: Long): Boolean = casVH(a, b)


  def unsafeCreateLocklessBoolean(initial: Boolean): AtomicVar[Boolean] =
    new AtomicBoolVH(initial) with Lockless[Boolean]:
      override def unsafeGet: Boolean = getVH
      override def unsafePut(s: Boolean): Unit = setVH(s)
      override def unsafeSwap(s: Boolean): Boolean = gasVH(s)
      override def unsafeCompareAndSet(a: Boolean, b: Boolean): Boolean = casVH(a, b)


  def unsafeCreateLocklessRef[S](initial: S): AtomicVar[S] =
    new AtomicRefVH(initial) with Lockless[S]:
      override def unsafeGet: S = getVH.asInstanceOf[S]
      override def unsafePut(s: S): Unit = setVH(s)
      override def unsafeSwap(s: S): S = gasVH(s).asInstanceOf[S]
      override def unsafeCompareAndSet(a: S, b: S): Boolean = casVH(a, b)


  private sealed trait Lockless[@specialized(Int, Long, Boolean) S] extends AtomicVar[S]:
    final override def isLocked: Boolean !! IO = !!.pure(false)
    final override def unsafeIsLocked(): Boolean = false

    def unsafeSwap(s: S): S
    def unsafeCompareAndSet(a: S, b: S): Boolean

    final def swap(s: S): S !! IO = !!.impure(unsafeSwap(s))
    final def modify(f: S => S): Unit !! IO = op(f, s => s, (_, _) => ())
    final def modifyGet(f: S => S): S !! IO = op(f, s => s, (_, s) => s)
    final def getModify(f: S => S): S !! IO = op(f, s => s, (s, _) => s)
    final def getModifyGet(f: S => S): (S, S) !! IO = op(f, s => s, (s1, s2) => (s1, s2))
    final def update[A](f: S => (A, S)): A !! IO = op(f, _._2, (_, as) => as._1)
    final def updateGet[A](f: S => (A, S)): (A, S) !! IO = op(f, _._2, (_, as) => as)
    final def getUpdate[A](f: S => (A, S)): (A, S) !! IO = op(f, _._2, (s, as) => (as._1, s))
    final def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! IO = op(f, _._2, (s, as) => (as._1, s, as._2))

    final def swapEff[U <: IO](s: S !! U): S !! U = opEff(_ => s, s => s, (s, _) => s)
    final def modifyEff[U <: IO](f: S => S !! U): Unit !! U = opEff(f, s => s, (_, _) => ())
    final def modifyGetEff[U <: IO](f: S => S !! U): S !! U = opEff(f, s => s, (_, s) => s)
    final def getModifyEff[U <: IO](f: S => S !! U): S !! U = opEff(f, s => s, (s, _) => s)
    final def getModifyGetEff[U <: IO](f: S => S !! U): (S, S) !! U = opEff(f, s => s, (s1, s2) => (s1, s2))
    final def updateEff[A, U <: IO](f: S => (A, S) !! U): A !! U = opEff(f, _._2, (_, as) => as._1)
    final def updateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U = opEff(f, _._2, (_, as) => as)
    final def getUpdateEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U = opEff(f, _._2, (s, as) => (as._1, s))
    final def getUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S, S) !! U = opEff(f, _._2, (s, as) => (as._1, s, as._2))

    final def trySwap(s: S): Option[S] !! IO = tryOp(_ => s, s => s, (s, _) => s)
    final def tryModify(f: S => S): Boolean !! IO = tryOp(f, s => s, (_, _) => ()).map(_.isDefined)
    final def tryModifyGet(f: S => S): Option[S] !! IO = tryOp(f, s => s, (_, s) => s)
    final def tryGetModify(f: S => S): Option[S] !! IO = tryOp(f, s => s, (s, _) => s)
    final def tryGetModifyGet(f: S => S): Option[(S, S)] !! IO = tryOp(f, s => s, (s1, s2) => (s1, s2))
    final def tryUpdate[A](f: S => (A, S)): Option[A] !! IO = tryOp(f, _._2, (_, as) => as._1)
    final def tryUpdateGet[A](f: S => (A, S)): Option[(A, S)] !! IO = tryOp(f, _._2, (_, as) => as)
    final def tryGetUpdate[A](f: S => (A, S)): Option[(A, S)] !! IO = tryOp(f, _._2, (s, as) => (as._1, s))
    final def tryGetUpdateGet[A](f: S => (A, S)): Option[(A, S, S)] !! IO = tryOp(f, _._2, (s, as) => (as._1, s, as._2))

    final def trySwapEff[U <: IO](s: S !! U): Option[S] !! U = tryOpEff(_ => s, s => s, (s, _) => s)
    final def tryModifyEff[U <: IO](f: S => S !! U): Boolean !! U = tryOpEff(f, s => s, (_, _) => ()).map(_.isDefined)
    final def tryModifyGetEff[U <: IO](f: S => S !! U): Option[S] !! U = tryOpEff(f, s => s, (_, s) => s)
    final def tryGetModifyEff[U <: IO](f: S => S !! U): Option[S] !! U = tryOpEff(f, s => s, (s, _) => s)
    final def tryGetModifyGetEff[U <: IO](f: S => S !! U): Option[(S, S)] !! U = tryOpEff(f, s => s, (s1, s2) => (s1, s2))
    final def tryUpdateEff[A, U <: IO](f: S => (A, S) !! U): Option[A] !! U = tryOpEff(f, _._2, (_, as) => as._1)
    final def tryUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): Option[(A, S)] !! U = tryOpEff(f, _._2, (_, as) => as)
    final def tryGetUpdateEff[A, U <: IO](f: S => (A, S) !! U): Option[(A, S)] !! U = tryOpEff(f, _._2, (s, as) => (as._1, s))
    final def tryGetUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): Option[(A, S, S)] !! U = tryOpEff(f, _._2, (s, as) => (as._1, s, as._2))


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


    private final inline def tryOp[A, B](inline f: S => A, inline g: A => S, inline h: (S, A) => B): Option[B] !! IO =
      !!.impure:
        val s = unsafeGet
        val a = f(s)
        val s2 = g(a)
        if unsafeCompareAndSet(s, s2) then
          Some(h(s, a))
        else
          None


    private final inline def tryOpEff[A, B, U <: IO](inline f: S => A !! U, inline g: A => S, inline h: (S, A) => B): Option[B] !! U =
      for
        s <- !!.impure(unsafeGet)
        a <- f(s)
        s2 = g(a)
        bb =
          if unsafeCompareAndSet(s, s2) then
            Some(h(s, a))
          else
            None
      yield bb


  final class Lockful[@specialized(Int, Long, Boolean) S] (_initial: S) extends AtomicVar[S]:
    @volatile private var currentValue: S = _initial
    private val lock: Mutex = Mutex.unsafeCreate()

    override def isLocked: Boolean !! IO = lock.isLocked
    override def unsafeIsLocked(): Boolean = lock.unsafeIsLocked()

    override def unsafeGet: S = currentValue
    override def unsafePut(s: S): Unit = currentValue = s

    override def swap(s: S): S !! IO = op(_ => s, s => s, (s, _) => s)
    override def modify(f: S => S): Unit !! IO = op(f, s => s, (_, _) => ())
    override def modifyGet(f: S => S): S !! IO = op(f, s => s, (_, s) => s)
    override def getModify(f: S => S): S !! IO = op(f, s => s, (s, _) => s)
    override def getModifyGet(f: S => S): (S, S) !! IO = op(f, s => s, (s1, s2) => (s1, s2))
    override def update[A](f: S => (A, S)): A !! IO = op(f, _._2, (_, as) => as._1)
    override def updateGet[A](f: S => (A, S)): (A, S) !! IO = op(f, _._2, (_, as) => as)
    override def getUpdate[A](f: S => (A, S)): (A, S) !! IO = op(f, _._2, (s, as) => (as._1, s))
    override def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! IO = op(f, _._2, (s, as) => (as._1, s, as._2))

    override def swapEff[U <: IO](s1: S !! U): S !! U = opEff(_ => s1, s => s, (s, _) => s)
    override def modifyEff[U <: IO](f: S => S !! U): Unit !! U = opEff(f, s => s, (_, _) => ())
    override def modifyGetEff[U <: IO](f: S => S !! U): S !! U = opEff(f, s => s, (_, s) => s)
    override def getModifyEff[U <: IO](f: S => S !! U): S !! U = opEff(f, s => s, (s, _) => s)
    override def getModifyGetEff[U <: IO](f: S => S !! U): (S, S) !! U = opEff(f, s => s, (s1, s2) => (s1, s2))
    override def updateEff[A, U <: IO](f: S => (A, S) !! U): A !! U = opEff(f, _._2, (_, as) => as._1)
    override def updateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U = opEff(f, _._2, (_, as) => as)
    override def getUpdateEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U = opEff(f, _._2, (s, as) => (as._1, s))
    override def getUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S, S) !! U = opEff(f, _._2, (s, as) => (as._1, s, as._2))

    override def trySwap(s: S): Option[S] !! IO = tryOp(_ => s, s => s, (s, _) => s)
    override def tryModify(f: S => S): Boolean !! IO = tryOp(f, s => s, (_, _) => ()).map(_.isDefined)
    override def tryModifyGet(f: S => S): Option[S] !! IO = tryOp(f, s => s, (_, s) => s)
    override def tryGetModify(f: S => S): Option[S] !! IO = tryOp(f, s => s, (s, _) => s)
    override def tryGetModifyGet(f: S => S): Option[(S, S)] !! IO = tryOp(f, s => s, (s1, s2) => (s1, s2))
    override def tryUpdate[A](f: S => (A, S)): Option[A] !! IO = tryOp(f, _._2, (_, as) => as._1)
    override def tryUpdateGet[A](f: S => (A, S)): Option[(A, S)] !! IO = tryOp(f, _._2, (_, as) => as)
    override def tryGetUpdate[A](f: S => (A, S)): Option[(A, S)] !! IO = tryOp(f, _._2, (s, as) => (as._1, s))
    override def tryGetUpdateGet[A](f: S => (A, S)): Option[(A, S, S)] !! IO = tryOp(f, _._2, (s, as) => (as._1, s, as._2))

    override def trySwapEff[U <: IO](s: S !! U): Option[S] !! U = tryOpEff(_ => s, s => s, (s, _) => s)
    override def tryModifyEff[U <: IO](f: S => S !! U): Boolean !! U = tryOpEff(f, s => s, (_, _) => ()).map(_.isDefined)
    override def tryModifyGetEff[U <: IO](f: S => S !! U): Option[S] !! U = tryOpEff(f, s => s, (_, s) => s)
    override def tryGetModifyEff[U <: IO](f: S => S !! U): Option[S] !! U = tryOpEff(f, s => s, (s, _) => s)
    override def tryGetModifyGetEff[U <: IO](f: S => S !! U): Option[(S, S)] !! U = tryOpEff(f, s => s, (s1, s2) => (s1, s2))
    override def tryUpdateEff[A, U <: IO](f: S => (A, S) !! U): Option[A] !! U = tryOpEff(f, _._2, (_, as) => as._1)
    override def tryUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): Option[(A, S)] !! U = tryOpEff(f, _._2, (_, as) => as)
    override def tryGetUpdateEff[A, U <: IO](f: S => (A, S) !! U): Option[(A, S)] !! U = tryOpEff(f, _._2, (s, as) => (as._1, s))
    override def tryGetUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): Option[(A, S, S)] !! U = tryOpEff(f, _._2, (s, as) => (as._1, s, as._2))

    private inline def op[A, B](inline f: S => A, inline g: A => S, inline h: (S, A) => B): B !! IO =
      lock.use:
        !!.impure:
          val s = currentValue
          val a = f(s)
          currentValue = g(a)
          h(s, a)

    private inline def opEff[A, B, U <: IO](inline f: S => A !! U, inline g: A => S, inline h: (S, A) => B): B !! U =
      lock.use:
        for
          s <- !!.impure(currentValue)
          a <- f(s)
          _ = { currentValue = g(a) }
        yield h(s, a)

    private inline def tryOp[A, B](inline f: S => A, inline g: A => S, inline h: (S, A) => B): Option[B] !! IO =
      lock.tryUse:
        !!.impure:
          val s = currentValue
          val a = f(s)
          currentValue = g(a)
          h(s, a)

    private inline def tryOpEff[A, B, U <: IO](inline f: S => A !! U, inline g: A => S, inline h: (S, A) => B): Option[B] !! U =
      lock.tryUse:
        for
          s <- !!.impure(currentValue)
          a <- f(s)
          _ = { currentValue = g(a) }
        yield h(s, a)

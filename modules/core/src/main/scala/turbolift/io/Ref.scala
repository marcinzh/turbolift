package turbolift.io
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import turbolift.!!


opaque type Ref[S] <: AnyRef = AtomicReference[S]


object Ref:
  def apply[S](initial: S): Ref[S] !! IO = IO(new AtomicReference(initial))

  extension [S](thiz: Ref[S])
    private def unwrap: AtomicReference[S] = thiz

    def get: S !! IO = IO(unwrap.get.nn)

    def gets[A](f: S => A): A !! IO = IO(f(unwrap.get.nn))

    def put(a: S): Unit !! IO = IO(unwrap.set(a))

    def swap(a: S): S !! IO = IO(thiz.getAndSet(a).nn)

    def modify(f: S => S): Unit !! IO =
      @tailrec def loop: Unit =
        val s = unwrap.get.nn
        val s2 = f(s)
        if unwrap.compareAndSet(s, s2)
        then ()
        else loop
      IO(loop)

    def modifyGet(f: S => S): S !! IO =
      @tailrec def loop: S =
        val s = unwrap.get.nn
        val s2 = f(s)
        if unwrap.compareAndSet(s, s2)
        then s2
        else loop
      IO(loop)

    def getModify(f: S => S): S !! IO =
      @tailrec def loop: S =
        val s = unwrap.get.nn
        val s2 = f(s)
        if unwrap.compareAndSet(s, s2)
        then s
        else loop
      IO(loop)

    def getModifyGet(f: S => S): (S, S) !! IO =
      @tailrec def loop: (S, S) =
        val s = unwrap.get.nn
        val s2 = f(s)
        if unwrap.compareAndSet(s, s2)
        then (s, s2)
        else loop
      IO(loop)

    def update[A](f: S => (A, S)): A !! IO =
      @tailrec def loop: A =
        val s = unwrap.get.nn
        val a_s2 = f(s)
        if unwrap.compareAndSet(s, a_s2._2)
        then a_s2._1
        else loop
      IO(loop)

    def updateGet[A](f: S => (A, S)): (A, S) !! IO =
      @tailrec def loop: (A, S) =
        val s = unwrap.get.nn
        val a_s2 = f(s)
        if unwrap.compareAndSet(s, a_s2._2)
        then a_s2
        else loop
      IO(loop)

    def getUpdate[A](f: S => (A, S)): (A, S) !! IO =
      @tailrec def loop: (A, S) =
        val s = unwrap.get.nn
        val a_s2 = f(s)
        if unwrap.compareAndSet(s, a_s2._2)
        then (a_s2._1, s)
        else loop
      IO(loop)

    def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! IO =
      @tailrec def loop: (A, S, S) =
        val s = unwrap.get.nn
        val a_s2 = f(s)
        if unwrap.compareAndSet(s, a_s2._2)
        then (a_s2._1, s, a_s2._2)
        else loop
      IO(loop)


    def tryModify(f: S => S): Boolean !! IO =
      IO {
        val s = unwrap.get.nn
        val s2 = f(s)
        unwrap.compareAndSet(s, s2)
      }

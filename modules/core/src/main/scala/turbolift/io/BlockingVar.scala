package turbolift.io
import turbolift.!!
import turbolift.effects.IO
import turbolift.internals.engine.concurrent.util.MutexImpl


/** Concurrent mutable variable.
 *
 * Like [[AtomicVar]], but blocks on contention instead spinning.
 */

final class BlockingVar[S](_initial: S) extends BlockingVar.Get[S] with BlockingVar.Put[S]:
  @volatile private var currentValue: S = _initial
  private val mutex: Mutex = new MutexImpl

  def asGet: BlockingVar.Get[S] = this
  def asPut: BlockingVar.Put[S] = this

  override def get: S !! IO = getModify(x => x)
  override def gets[A](f: S => A): A !! IO = get.map(f)
  override def put(s: S): Unit !! IO = modify(_ => s)

  def swap(s0: S): S !! IO = op(_ => s0, s => s, (s, _) => s)
  def modify(f: S => S): Unit !! IO = op(f, s => s, (_, _) => ())
  def modifyGet(f: S => S): S !! IO = op(f, s => s, (_, s) => s)
  def getModify(f: S => S): S !! IO = op(f, s => s, (s, _) => s)
  def getModifyGet(f: S => S): (S, S) !! IO = op(f, s => s, (s1, s2) => (s1, s2))
  def update[A](f: S => (A, S)): A !! IO = op(f, _._2, (_, as) => as._1)
  def updateGet[A](f: S => (A, S)): (A, S) !! IO = op(f, _._2, (_, as) => as)
  def getUpdate[A](f: S => (A, S)): (A, S) !! IO = op(f, _._2, (s, as) => (as._1, s))
  def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! IO = op(f, _._2, (s, as) => (as._1, s, as._2))

  def swapEff[U <: IO](ss: S !! U): S !! U = opEff(_ => ss, s => s, (s, _) => s)
  def modifyEff[U <: IO](f: S => S !! U): Unit !! U = opEff(f, s => s, (_, _) => ())
  def modifyGetEff[U <: IO](f: S => S !! U): S !! U = opEff(f, s => s, (_, s) => s)
  def getModifyEff[U <: IO](f: S => S !! U): S !! U = opEff(f, s => s, (s, _) => s)
  def getModifyGetEff[U <: IO](f: S => S !! U): (S, S) !! U = opEff(f, s => s, (s1, s2) => (s1, s2))
  def updateEff[A, U <: IO](f: S => (A, S) !! U): A !! U = opEff(f, _._2, (_, as) => as._1)
  def updateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U = opEff(f, _._2, (_, as) => as)
  def getUpdateEff[A, U <: IO](f: S => (A, S) !! U): (A, S) !! U = opEff(f, _._2, (s, as) => (as._1, s))
  def getUpdateGetEff[A, U <: IO](f: S => (A, S) !! U): (A, S, S) !! U = opEff(f, _._2, (s, as) => (as._1, s, as._2))


  private inline def op[A, B](inline f: S => A, inline g: A => S, inline h: (S, A) => B): B !! IO =
    mutex.lock:
      !!.impure:
        val s = currentValue
        val a = f(s)
        currentValue = g(a)
        h(s, a)

  private inline def opEff[A, B, U <: IO](inline f: S => A !! U, inline g: A => S, inline h: (S, A) => B): B !! U =
    mutex.lock:
      for
        s <- !!.impure(currentValue)
        a <- f(s)
        _ = { currentValue = g(a) }
      yield h(s, a)


object BlockingVar:
  sealed trait Get[S]:
    def get: S !! IO
    def gets[A](f: S => A): A !! IO


  sealed trait Put[S]:
    def put(s: S): Unit !! IO


  def apply[S](initial: S): BlockingVar[S] !! IO = create(initial)
  def create[S](initial: S): BlockingVar[S] !! IO = !!.impure(new BlockingVar(initial))

package turbolift.interpreter
import turbolift.!!
import turbolift.internals.primitives.{ComputationCases => CC}
import turbolift.interpreter.Void

/** Access to effect's local state. */

final class Local[S, V](private[turbolift] val interp: Interpreter.Untyped):
  private final val getAny = CC.LocalGet(interp)
  def get: S !! V = getAny.asInstanceOf[S !! V]
  def gets[A](f: S => A): A !! V = get.map(f)
  def put(s2: S): Unit !! V = CC.LocalPut(interp, s2)
  def swap(s2: S): S !! V = update(s => (s, s2))
  def modify(f: S => S): Unit !! V = update(s => ((), f(s)))
  def modifyGet(f: S => S): S !! V = update(s => { val s2 = f(s); (s2, s2) })
  def getModify(f: S => S): S !! V = update(s => { val s2 = f(s); (s, s2) })
  def getModifyGet(f: S => S): (S, S) !! V = update(s => { val s2 = f(s); ((s, s2), s2) })
  def update[A](f: S => (A, S)): A !! V = CC.LocalUpdate(interp, f)
  def updateGet[A](f: S => (A, S)): (A, S) !! V = update(s => { val xs2 @ (_, s2) = f(s); (xs2, s2) })
  def getUpdate[A](f: S => (A, S)): (A, S) !! V = update(s => { val (x, s2) = f(s); ((x, s), s2) })
  def getUpdateGet[A](f: S => (A, S)): (A, S, S) !! V = update(s => { val (x, s2) = f(s); ((x, s, s2), s2) })

package turbolift.interpreter
import turbolift.!!
import turbolift.internals.primitives.{ComputationCases => CC}
import turbolift.interpreter.Void

/** Local state of effect.
 *
 * This is a **Primitive Effect**, provided for implementing custom effects:
 *
 * 1. It's accessible only from within custom implementations of [[Interpreter]]s.
 *    Custom effects can invoke [[Local]]'s operations to implement their own operations.
 *
 * 2. It does not require a handler.
 *    Invoking [[Local]]'s operations does not manifest as a dependency.
 *
 * See also another primitive effect: [[Control]].
 *
 * @tparam S local state
 * @tparam V effect set
*/

final class Local[S, V] private[interpreter] (private val interp: Interpreter.Untyped):
  private final val getAny = CC.LocalGet(interp)
  def get: S !! V = getAny.asInstanceOf[S !! V]
  def gets[A](f: S => A): A !! V = get.map(f)
  def put(s2: S): Unit !! V = CC.LocalPut(interp, s2)
  inline def swap(s2: S): S !! V = update(s => (s, s2))
  inline def modify(inline f: S => S): Unit !! V = update(s => ((), f(s)))
  inline def modifyGet(inline f: S => S): S !! V = update(s => { val s2 = f(s); (s2, s2) })
  inline def getModify(inline f: S => S): S !! V = update(s => { val s2 = f(s); (s, s2) })
  inline def getModifyGet(inline f: S => S): (S, S) !! V = update(s => { val s2 = f(s); ((s, s2), s2) })
  inline def updateGet[A](inline f: S => (A, S)): (A, S) !! V = update(s => { val xs2 @ (_, s2) = f(s); (xs2, s2) })
  inline def getUpdate[A](inline f: S => (A, S)): (A, S) !! V = update(s => { val (x, s2) = f(s); ((x, s), s2) })
  inline def getUpdateGet[A](inline f: S => (A, S)): (A, S, S) !! V = update(s => { val (x, s2) = f(s); ((x, s, s2), s2) })

  inline def update[A](inline f: S => (A, S)): A !! V =
    new CC.LocalUpdate[A, S](interp):
      override def apply(s: S): (A, S) = f(s)

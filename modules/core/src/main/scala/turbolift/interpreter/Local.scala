package turbolift.interpreter
import turbolift.!!
import turbolift.interpreter.Void
import turbolift.{ComputationCases => CC}

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

final class Local[S, V] private[interpreter] (private val prompt: Prompt):
  private final val getAny = CC.LocalGet(prompt)
  def get: S !! V = getAny.asInstanceOf[S !! V]
  def gets[A](f: S => A): A !! V = get.map(f)
  def put(s2: S): Unit !! V = CC.LocalPut(prompt, s2)
  inline def swap(s2: S): S !! V = update(s => (s, s2))
  inline def modify(inline f: S => S): Unit !! V = update(s => ((), f(s)))
  inline def modifyGet(inline f: S => S): S !! V = update(s => { val s2 = f(s); (s2, s2) })
  inline def getModify(inline f: S => S): S !! V = update(s => { val s2 = f(s); (s, s2) })
  inline def getModifyGet(inline f: S => S): (S, S) !! V = update(s => { val s2 = f(s); ((s, s2), s2) })
  inline def update[A](inline f: S => (A, S)): A !! V = CC.localUpdate(prompt, f)
  inline def updateGet[A](inline f: S => (A, S)): (A, S) !! V = update(s => { val xs2 @ (_, s2) = f(s); (xs2, s2) })
  inline def getUpdate[A](inline f: S => (A, S)): (A, S) !! V = update(s => { val (x, s2) = f(s); ((x, s), s2) })
  inline def getUpdateGet[A](inline f: S => (A, S)): (A, S, S) !! V = update(s => { val (x, s2) = f(s); ((x, s, s2), s2) })

  def getsEff[A, U <: V](f: S => A !! U): A !! U = get.flatMap(f)
  inline def modifyEff[U <: V](inline f: S => S !! U): Unit !! U = get.flatMap(f).flatMap(put)
  inline def modifyGetEff[U <: V](inline f: S => S !! U): S !! U = get.flatMap(f).flatMap(s2 => put(s2).as(s2))
  inline def getModifyEff[U <: V](inline f: S => S !! U): S !! U = get.flatMap(s => f(s).flatMap(s2 => put(s2).as(s)))
  inline def getModifyGetEff[U <: V](inline f: S => S !! U): (S, S) !! U = get.flatMap(s => f(s).flatMap(s2 => put(s2).as((s, s2))))
  inline def updateEff[A, U <: V](inline f: S => (A, S) !! U): A !! U = get.flatMap(f).flatMap { case (a, s2) => put(s2).as(a) }
  inline def updateGetEff[A, U <: V](inline f: S => (A, S) !! U): (A, S) !! U = get.flatMap(f).flatMap { case a_s2@(_, s2) => put(s2).as(a_s2) }
  inline def getUpdateEff[A, U <: V](inline f: S => (A, S) !! U): (A, S) !! U = get.flatMap(s => f(s).flatMap { case (a, s2) => put(s2).as((a, s)) })
  inline def getUpdateGetEff[A, U <: V](inline f: S => (A, S) !! U): (A, S, S) !! U = get.flatMap(s => f(s).flatMap { case (a, s2) => put(s2).as((a, s, s2)) })

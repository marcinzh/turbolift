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
  inline def modify(inline f: S => S): Unit !! V = CC.localModify(prompt, f)
  inline def modifyGet(inline f: S => S): S !! V = CC.localUpdate(prompt, f, (s, _) => s, s => s)
  inline def getModify(inline f: S => S): S !! V = CC.localUpdate(prompt, f, (_, s) => s, s => s)
  inline def getModifyGet(inline f: S => S): (S, S) !! V = CC.localUpdate(prompt, f, (s2, s1) => (s1, s2), s => s)
  inline def update[A](inline f: S => (A, S)): A !! V = CC.localUpdate(prompt, f, (a_s2, _) => a_s2._1, _._2)
  inline def updateGet[A](inline f: S => (A, S)): (A, S) !! V = CC.localUpdate(prompt, f, (a_s2, _) => a_s2, _._2)
  inline def getUpdate[A](inline f: S => (A, S)): (A, S) !! V = CC.localUpdate(prompt, f, (a_s2, s1) => (a_s2._1, s1), _._2)
  inline def getUpdateGet[A](inline f: S => (A, S)): (A, S, S) !! V = CC.localUpdate(prompt, f, (a_s2, s1) => (a_s2._1, s1, a_s2._2), _._2)

  inline def getsEff[A, U <: V](f: S => A !! U): A !! U = CC.localGetsEff(prompt, f)
  inline def putEff[U <: V](ss: S !! U): Unit !! U = ss.flatMap(put)
  inline def swapEff[U <: V](ss: S !! U): S !! U = ss.flatMap(swap)
  inline def modifyEff[U <: V](inline f: S => S !! U): Unit !! U = getsEff(f).flatMap(put)
  inline def modifyGetEff[U <: V](inline f: S => S !! U): S !! U = getsEff(f).flatMap(s => put(s).as(s))
  inline def getModifyEff[U <: V](inline f: S => S !! U): S !! U = getsEff(s => f(s).flatMap(put).as(s))
  inline def getModifyGetEff[U <: V](inline f: S => S !! U): (S, S) !! U = getsEff(s => f(s).flatMap(s2 => put(s2).as((s, s2))))
  inline def updateEff[A, U <: V](inline f: S => (A, S) !! U): A !! U = getsEff(f).flatMap { case (a, s2) => put(s2).as(a) }
  inline def updateGetEff[A, U <: V](inline f: S => (A, S) !! U): (A, S) !! U = getsEff(f).flatMap { case a_s2@(_, s2) => put(s2).as(a_s2) }
  inline def getUpdateEff[A, U <: V](inline f: S => (A, S) !! U): (A, S) !! U = getsEff(s => f(s).flatMap { case (a, s2) => put(s2).as((a, s)) })
  inline def getUpdateGetEff[A, U <: V](inline f: S => (A, S) !! U): (A, S, S) !! U = getsEff(s => f(s).flatMap { case (a, s2) => put(s2).as((a, s, s2)) })

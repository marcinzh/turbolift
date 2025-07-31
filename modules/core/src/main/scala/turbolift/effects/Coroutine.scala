package turbolift.effects
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import CoroutineEffect.Step


/** Signature of [[CoroutineEffect]]. */
trait CoroutineSignature[I, O, R] extends Signature:
  def yeld(value: O): I !! ThisEffect
  def exit(value: R): Nothing !! ThisEffect


/** Base trait for custom instances of Coroutine effect.
 *
 * {{{
 * case object MyCoroutine extends CoroutineEffect[Int, String, Unit]
 * // optional:
 * type MyCoroutine = MyCoroutine.type
 * }}}
 *
 * @see [[turbolift.io.Coroutine]]
 *
 * @tparam I value passed to the coroutine when it's resumed
 * @tparam O value returned by the coroutine when it suspends
 * @tparam R value returned by the coroutine when it ends
 */
trait CoroutineEffect[I, O, R] extends Effect[CoroutineSignature[I, O, R]] with CoroutineSignature[I, O, R]:
  enclosing =>
  final override def yeld(value: O): I !! this.type = perform(_.yeld(value))
  final override def exit(value: R): Nothing !! this.type = perform(_.exit(value))

  final def handler[U]: Handler[Const[R], Const[Step[I, O, R, U & enclosing.type]], enclosing.type, U] =
    type SS = Step[I, O, R, U & enclosing.type]
    new impl.Stateless[Const[R], Const[SS], U] with impl.Sequential with CoroutineSignature[I, O, R]:
      override def captureHint = true
      override def onReturn(r: R) = Step.Exit(r).pure_!!
      override def yeld(o: O) = Control.capture0(k => !!.pure(Step.Yield(o, i => Control.strip(k(i)))))
      override def exit(r: R) = Control.abort(Step.Exit(r))
    .toHandler


object CoroutineEffect:
  enum Step[I, O, R, U]:
    case Exit(r: R)
    case Yield(o: O, k: I => R !! U)

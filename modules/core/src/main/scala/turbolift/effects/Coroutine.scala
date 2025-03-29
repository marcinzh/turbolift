package turbolift.effects
import turbolift.{!!, Signature, Effect}
import turbolift.Extensions._
import CoroutineEffect.Step


trait CoroutineSignature[I, O, R] extends Signature:
  def yeld(value: O): I !! ThisEffect
  def exit(value: R): Nothing !! ThisEffect


trait CoroutineEffect[I, O, R] extends Effect[CoroutineSignature[I, O, R]] with CoroutineSignature[I, O, R]:
  enclosing =>
  final override def yeld(value: O): I !! this.type = perform(_.yeld(value))
  final override def exit(value: R): Nothing !! this.type = perform(_.exit(value))

  type Fx = this.type

  final def handler[U]: ThisHandler[Const[R], Const[Step[I, O, R, U & Fx]], Any] =
    type SS = Step[I, O, R, U & Fx]
    new impl.Stateless[Const[R], Const[SS], Any] with impl.Sequential with CoroutineSignature[I, O, R]:
      override def captureHint = true
      override def onReturn(r: R) = Step.Exit(r).pure_!!
      override def yeld(o: O) = Control.capture0(k => !!.pure(Step.Yield(o, i => Control.strip(k(i)))))
      override def exit(r: R) = Control.abort(Step.Exit(r))
    .toHandler


object CoroutineEffect:
  enum Step[I, O, R, U]:
    case Exit(r: R)
    case Yield(o: O, k: I => R !! U)

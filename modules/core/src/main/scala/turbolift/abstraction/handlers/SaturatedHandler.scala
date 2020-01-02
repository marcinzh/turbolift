package turbolift.abstraction.handlers
import turbolift.abstraction.!!
import turbolift.abstraction.ComputationCases.PushHandler


private[abstraction] sealed trait SaturatedHandler extends HandlerCases.Unsealed {
  type Trans[M[_], A]
  // override type Effects <: AnyRef

  val primitive: PrimitiveHandler[Trans, Result]
  def prime[M[_], A](tma: Trans[M, A]): M[Result[A]]

  final def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U = 
    new PushHandler[A, U, this.type](eff, this)
}


object SaturatedHandler {
  trait Nullary[Fx, O[_]] extends PrimitiveHandler.Nullary[O] with SaturatedHandler {
    final override type Effects = Fx
    final override type Result[A] = O[A]
    final override type Trans[M[_], A] = M[O[A]]

    final override val primitive = this
    final override def prime[M[_], A](tma: M[O[A]]): M[O[A]] = tma
  }

  trait Unary[Fx, S, O[_]] extends PrimitiveHandler.Unary[S, O] {
    def apply(initial: S): ThisSaturatedHandler = new ThisSaturatedHandler(initial)

    class ThisSaturatedHandler(initial: S) extends SaturatedHandler {
      final override type Effects = Fx
      final override type Result[A] = O[A]
      final override type Trans[M[_], A] = S => M[O[A]]

      final override val primitive = Unary.this
      final override def prime[M[_], A](tma: S => M[O[A]]): M[O[A]] = tma(initial)
    }
  }
}

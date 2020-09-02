package turbolift.abstraction.internals.handler
import turbolift.abstraction.{!!, HandlerCases}
import turbolift.abstraction.ComputationCases.Scope


private[abstraction] sealed trait SaturatedHandler extends HandlerCases.Unsealed {
  type Trans[M[_], A]

  val primitive: PrimitiveHandler[Trans, Result]
  def prime[M[_], A](tma: Trans[M, A]): M[Result[A]]

  final def doHandle[A, U](comp: A !! U with Effects): Result[A] !! U = 
    new Scope[A, U, this.type](comp, this)
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

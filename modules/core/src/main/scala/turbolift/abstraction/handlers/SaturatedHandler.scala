package turbolift.abstraction.handlers
import turbolift.abstraction.!!
import turbolift.abstraction.ComputationCases.HandleInScope
import mwords._


private[abstraction] trait SaturatedHandler extends Handler.Unsealed {
  type Trans[M[+_], +A]
  override type Effects <: AnyRef

  val primitive: PrimitiveHandler[Trans]
  def prime[M[+_], A](tma: Trans[M, A]): M[Result[A]]

  final def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U =
    new HandleInScope[A, U, this.type](eff, this)
}

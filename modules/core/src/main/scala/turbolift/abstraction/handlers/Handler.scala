package turbolift.abstraction.handlers
import turbolift.abstraction.!!
import turbolift.abstraction.handlers.aux.CanHandle
import HandlerCases._
import mwords._


sealed trait Handler {
  type Result[A]
  type Effects
  final type This = Handler.Apply[Result, Effects]
  final type Into[F[_]] = Result ~> F

  final def run[A](eff: A !! Effects): Result[A] = handle[Any](eff).run

  final def handle[V] = new HandleApply[V]
  final class HandleApply[V] {
    def apply[A, W](eff: A !! W)(implicit ev: CanHandle[V, W, Effects]): Result[A] !! V =
      doHandle[A, V](ev(eff))
  }

  final def <<<![H <: Handler](that: H) = Composed[This, H](this, that)
  final def >>>![H <: Handler](that: H) = that <<<! this

  final def map[F[_]](f: Result ~> F): Handler.Apply[F, Effects] = Mapped[This, F](this)(f)

  protected[abstraction] def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U
}


object Handler {
  type Apply[F[_], U] = Handler {
    type Result[A] = F[A]
    type Effects = U
  }
}


object HandlerCases {
  private[abstraction] trait Unsealed extends Handler

  final case class Composed[HL <: Handler, HR <: Handler](val lhs: HL, val rhs: HR) extends Handler {
    override type Effects = lhs.Effects with rhs.Effects
    override type Result[A] = lhs.Result[rhs.Result[A]]

    protected[abstraction] override def doHandle[A, U](eff: A !! lhs.Effects with rhs.Effects with U): Result[A] !! U =
      lhs.doHandle[rhs.Result[A], U](
        rhs.doHandle[A, U with lhs.Effects](eff)
      )
  }

  final case class Mapped[H <: Handler, F[_]](that: H)(fun: H#Result ~> F) extends Handler {
    override type Result[A] = F[A]
    override type Effects = that.Effects

    protected[abstraction] override def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U =
      that.doHandle[A, U](eff).map(fun(_))
  }
}


trait HandlerExports {
  type >>>![H1 <: Handler, H2 <: Handler] = H2 <<<! H1

  type <<<![H1 <: Handler, H2 <: Handler] = Handler {
    type Effects = H1#Effects with H2#Effects
    type Result[A] = H1#Result[H2#Result[A]]
  }

  implicit class HandlerExtension[S, U](val thiz: Handler.Apply[(S, ?), U]) {
    type Const[X] = S

    def eval: Handler.Apply[Identity, U] = thiz.map(new ((S, ?) ~> Identity) {
      def apply[A](pair: (S, A)) = pair._2
    })

    def exec: Handler.Apply[Const, U] = thiz.map[Const](new ((S, ?) ~> Const) {
      def apply[A](pair: (S, A)) = pair._1
    })

    def justState = exec
    def dropState = eval
  }
}

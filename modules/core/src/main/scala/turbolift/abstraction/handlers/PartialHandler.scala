package turbolift.abstraction.handlers
import turbolift.abstraction.!!
import turbolift.abstraction.ComputationCases.HandleInScope
import turbolift.abstraction.handlers.aux.CanHandle
import mwords._


sealed trait PartialHandler { outer =>
  type Result[+A]
  type Effects

  protected[abstraction] def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U

  final def run[A](eff: A !! Effects): Result[A] = handle[Any](eff).run

  final def handle[V] = new HandleApply[V]
  final class HandleApply[V] {
    def apply[A, W](eff: A !! W)(implicit ev: CanHandle[V, W, Effects]): Result[A] !! V =
      doHandle[A, V](ev(eff))
  }


  final def <<<![H <: PartialHandler](that: H) = new Composed(that)
  final def >>>![H <: PartialHandler](that: H) = that <<<! this

  final def map[F[+_]](f: Result ~> F): PartialHandler.Apply[F, Effects] = new Mapped[F] {
    def apply[A](x: outer.Result[A]): F[A] = f.apply[A](x)
  }
  
  final class Composed[H <: PartialHandler](val h: H) extends PartialHandler {
    override type Effects = outer.Effects with h.Effects
    override type Result[+A] = outer.Result[h.Result[A]]

    protected[abstraction] override def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U =
      outer.doHandle[h.Result[A], U](
        h.doHandle[A, U with outer.Effects](eff)
      )
  }

  abstract class Mapped[F[+_]] extends PartialHandler {
    override type Result[+A] = F[A]
    override type Effects = outer.Effects

    protected[abstraction] override def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U =
      outer.doHandle[A, U](eff).map(apply(_))

    def apply[A](x: outer.Result[A]): F[A]
  }
}


object PartialHandler {
  type Apply[F[+_], U] = PartialHandler {
    type Result[+A] = F[A]
    type Effects = U
  }

  private[abstraction] trait Gimmick extends PartialHandler { outer =>
    type Trans[M[+_], +A]
    override type Effects <: AnyRef

    val primitive: PrimitiveHandler[Trans]
    def gimmick[M[+_], A](tma: Trans[M, A]): M[Result[A]]

    final def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U =
      new HandleInScope[A, U, this.type](eff, this)
  }
}


trait PartialHandlerExports {
  type >>>![H1 <: PartialHandler, H2 <: PartialHandler] = H2 <<<! H1

  type <<<![H1 <: PartialHandler, H2 <: PartialHandler] = PartialHandler {
    type Effects = H1#Effects with H2#Effects
    type Result[+A] = H1#Result[H2#Result[A]]
  }

  implicit class PartialHandlerExtension[S, U](val thiz: PartialHandler.Apply[(S, +?), U]) {
    def eval: PartialHandler.Apply[Identity, U] = new thiz.Mapped[Identity] {
      def apply[A](pair: (S, A)) = pair._2
    }

    def exec: PartialHandler.Apply[Lambda[+[X] => S], U] = new thiz.Mapped[Lambda[+[X] => S]] {
      def apply[A](pair: (S, A)) = pair._1
    }

    def justState = exec
    def dropState = eval
  }
}

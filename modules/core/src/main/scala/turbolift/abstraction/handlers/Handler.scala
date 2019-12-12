package turbolift.abstraction.handlers
import turbolift.abstraction.!!
import turbolift.abstraction.handlers.aux.CanHandle
import mwords._


sealed trait Handler { outer =>
  type Result[+A]
  type Effects

  protected[abstraction] def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U

  final def run[A](eff: A !! Effects): Result[A] = handle[Any](eff).run

  final def handle[V] = new HandleApply[V]
  final class HandleApply[V] {
    def apply[A, W](eff: A !! W)(implicit ev: CanHandle[V, W, Effects]): Result[A] !! V =
      doHandle[A, V](ev(eff))
  }


  final def <<<![H <: Handler](that: H) = new Composed(that)
  final def >>>![H <: Handler](that: H) = that <<<! this

  final def map[F[+_]](f: Result ~> F): Handler.Apply[F, Effects] = new Mapped[F] {
    def apply[A](x: outer.Result[A]): F[A] = f.apply[A](x)
  }
  
  final class Composed[H <: Handler](val h: H) extends Handler {
    override type Effects = outer.Effects with h.Effects
    override type Result[+A] = outer.Result[h.Result[A]]

    protected[abstraction] override def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U =
      outer.doHandle[h.Result[A], U](
        h.doHandle[A, U with outer.Effects](eff)
      )
  }

  abstract class Mapped[F[+_]] extends Handler {
    override type Result[+A] = F[A]
    override type Effects = outer.Effects

    protected[abstraction] override def doHandle[A, U](eff: A !! Effects with U): Result[A] !! U =
      outer.doHandle[A, U](eff).map(apply(_))

    def apply[A](x: outer.Result[A]): F[A]
  }
}


object Handler {
  type Apply[F[+_], U] = Handler {
    type Result[+A] = F[A]
    type Effects = U
  }

  private[abstraction] trait Unsealed extends Handler
}


trait HandlerExports {
  type >>>![H1 <: Handler, H2 <: Handler] = H2 <<<! H1

  type <<<![H1 <: Handler, H2 <: Handler] = Handler {
    type Effects = H1#Effects with H2#Effects
    type Result[+A] = H1#Result[H2#Result[A]]
  }

  implicit class HandlerExtension[S, U](val thiz: Handler.Apply[(S, +?), U]) {
    def eval: Handler.Apply[Identity, U] = new thiz.Mapped[Identity] {
      def apply[A](pair: (S, A)) = pair._2
    }

    def exec: Handler.Apply[Lambda[+[X] => S], U] = new thiz.Mapped[Lambda[+[X] => S]] {
      def apply[A](pair: (S, A)) = pair._1
    }

    def justState = exec
    def dropState = eval
  }
}

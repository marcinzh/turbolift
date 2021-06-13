package turbolift.abstraction
import turbolift.abstraction.internals.engine.MainLoop
import turbolift.abstraction.internals.aux.{CanRun, CanTotallyHandle, CanPartiallyHandle}


private [abstraction] trait ComputationExtensions {
  implicit class ComputationExtension[A, U](thiz: Computation[A, U]) {
    def run(implicit ev: CanRun[U]): A = MainLoop.pure(ev(thiz)).run
    def runStackUnsafe(implicit ev: CanRun[U]): A = MainLoop.pureStackUnsafe[A](ev(thiz))

    def runWith[F[_], L](h: IHandler[F, L])(implicit ev: CanTotallyHandle[U, L]): F[A] =
      h.doHandle[A, Any](ev(thiz)).run

    def runStackUnsafeWith[F[_], L, N](h: IHandler[F, L])(implicit ev: CanTotallyHandle[U, L]): F[A] =
      h.doHandle[A, Any](ev(thiz)).runStackUnsafe

    def handleWith[V] : HandleWithApply[V] = new HandleWithApply[V]
    class HandleWithApply[V] {
      def apply[F[_], L, N, V2 <: V with N](h: Handler[F, L, N])(implicit ev: CanPartiallyHandle[V, U, L]): F[A] !! V2 =
        h.doHandle[A, V](ev(thiz))
    }

    def downCast[V >: U] = thiz.asInstanceOf[Computation[A, V]]
  }

  implicit class ComputationOfPairExtension[A, B, U](thiz: Computation[(A, B), U]) {
    def map2[C](f: (A, B) => C): C !! U = thiz.map(f.tupled)
    def flatMap2[C, V <: U](f: (A, B) => C !! V): C !! V = thiz.flatMap(f.tupled)
  }
}

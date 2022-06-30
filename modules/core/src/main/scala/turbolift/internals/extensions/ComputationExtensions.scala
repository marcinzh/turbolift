package turbolift.internals.extensions
import turbolift.{!!, Computation, Handler}
import turbolift.internals.engine.MainLoop
import turbolift.internals.aux.{CanRun, CanTotallyHandle, CanPartiallyHandle}


trait ComputationExtensions:
  extension [A, U](thiz: Computation[A, U])
    def run(implicit ev: CanRun[U]): A = MainLoop.pure.run(ev(thiz)).run

    def runWith[F[+_], L](h: Handler.Free[F, L])(implicit ev: CanTotallyHandle[U, L]): F[A] =
      h.doHandle[A, Any](ev(thiz)).run

    def downCast[U2 >: U] = thiz.asInstanceOf[Computation[A, U2]]
  
    def >>=![F[+_], L, N](f: A => Handler[F, L, N]): Handler[F, L, U & N] = Handler.flatten(thiz.map(f))


  implicit class ComputationExtensions[A, U](thiz: Computation[A, U]):
    def handleWith[V]: HandleWithApply[V] = new HandleWithApply[V]
    class HandleWithApply[V]:
      def apply[F[+_], L, N, V2 <: V & N](h: Handler[F, L, N])(implicit ev: CanPartiallyHandle[V, U, L]): F[A] !! V2 =
        h.doHandle[A, V](ev(thiz))


  extension [F[+_], L, N](thiz: Computation[Handler[F, L, N], N])
    def flattenHandler: Handler[F, L, N] = Handler.flatten(thiz)


  extension [A, B, U](thiz: Computation[(A, B), U])
    def map2[C](f: (A, B) => C): C !! U = thiz.map(f.tupled)
    def flatMap2[C, U2 <: U](f: (A, B) => C !! U2): C !! U2 = thiz.flatMap(f.tupled)


  extension [U](thiz: Computation[Boolean, U])
    def while_!![U2 <: U](body: => Unit !! U2): Unit !! U2 = !!.repeatWhile(thiz)(body)
    def until_!![U2 <: U](body: => Unit !! U2): Unit !! U2 = !!.repeatUntil(thiz)(body)

    def if_!![U2 <: U](thenBody: => Unit !! U2)(elseBody: => Unit !! U2): Unit !! U2 =
      thiz.flatMap(if _ then thenBody else elseBody)

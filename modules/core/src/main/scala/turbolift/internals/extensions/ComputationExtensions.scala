package turbolift.internals.extensions
import scala.util.Try
import turbolift.{!!, Computation, Handler}
import turbolift.io.IO
import turbolift.internals.launcher.{Launcher, LauncherConfig}
import turbolift.internals.auxx.{CanRun, CanUnsafeRun, CanPartiallyHandle}


/** No need to use this trait directly, because it's inherited by [[turbolift.Computation Computation]]'s companion object. */
/*private[turbolift]*/ trait ComputationExtensions:
  extension [A](thiz: Computation[A, Any])
    /** Runs the computation, provided that it requests no effects. */
    def run(using config: LauncherConfig = LauncherConfig.default): A = Launcher.run(thiz).get


  extension [A, U >: IO](thiz: Computation[A, U])
    /** Runs the computation, provided that it requests IO effect only, or none at all. */
    def unsafeRun(using config: LauncherConfig = LauncherConfig.default): Try[A] = Launcher.run(thiz)


  extension [A, U](thiz: Computation[A, U])
    def downCast[U2 >: U] = thiz.asInstanceOf[Computation[A, U2]]
  
    /** Simplifies effectful creation of handlers.
     * 
     *  Passes computed value to handler constructor.
     *  Effect used to compute the value, are absorbed by the handler, into its own dependencies.
     */ 
    def >>=![F[+_], L, N](f: A => Handler[F, L, N]): Handler[F, L, U & N] = Handler.flatHandle(thiz.map(f))

    /** Applies a handler to this computation.
     *
     *  Same as `myHandler.handle(this)`.
     */
    def handleWith[V]: HandleWithApply[A, U, V] = new HandleWithApply[A, U, V](thiz)


  extension [F[+_], L, N](thiz: Computation[Handler[F, L, N], N])
    /** Simplifies effectful creation of handlers.
     * 
     *  Same as [[turbolift.Handler.flatHandle Handler.flatHandle(this)]].
     */
    def flattenHandler: Handler[F, L, N] = Handler.flatHandle(thiz)


  extension [A, B, U](thiz: Computation[(A, B), U])
    def map2[C](f: (A, B) => C): C !! U = thiz.map(f.tupled)
    def flatMap2[C, U2 <: U](f: (A, B) => C !! U2): C !! U2 = thiz.flatMap(f.tupled)


  extension [U](thiz: Computation[Boolean, U])
    /** Like `while` statement, but the condition and the body are computations. */
    def while_!![U2 <: U](body: => Unit !! U2): Unit !! U2 = !!.repeatWhile(thiz)(body)

    /** Like `while` statement, but the condition and the body are computations. */
    def until_!![U2 <: U](body: => Unit !! U2): Unit !! U2 = !!.repeatUntil(thiz)(body)

    /** Like `if` statement, but the condition and the body are computations. */
    def if_!![U2 <: U](thenBody: => Unit !! U2)(elseBody: => Unit !! U2): Unit !! U2 =
      thiz.flatMap(if _ then thenBody else elseBody)


private[turbolift] class HandleWithApply[A, U, V](thiz: A !! U):
  def apply[F[+_], L, N, V2 <: V & N](h: Handler[F, L, N])(implicit ev: CanPartiallyHandle[V, U, L]): F[A] !! V2 =
    h.doHandle[A, V](ev(thiz))

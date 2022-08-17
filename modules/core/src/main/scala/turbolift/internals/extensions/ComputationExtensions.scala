package turbolift.internals.extensions
import turbolift.{!!, Computation, Handler}
import turbolift.internals.launcher.{Launcher, LauncherConfig, LauncherConfigs}
import turbolift.internals.auxx.{CanRun, CanPartiallyHandle}


private[turbolift] trait ComputationExtensions:
  extension [A, U](thiz: Computation[A, U])
    /** Runs the computation, provided that it requests no effects. */
    def run(using ev: CanRun[U], config: LauncherConfig = LauncherConfigs.MT): A = Launcher.sync.unsafeGet.run(ev(thiz))

    /** Currently: like `run`, but captures leaked exceptions. True `IO` is WIP. */
    def unsafeRun(using ev: CanRun[U], config: LauncherConfig = LauncherConfigs.MT) = Launcher.sync.run(ev(thiz))

    // def runST(using ev: CanRun[U]): A = Launcher.sync(using LauncherConfigs.ST).unsafeGet.run(ev(thiz))
    // def runMT(using ev: CanRun[U]): A = Launcher.sync(using LauncherConfigs.MT).unsafeGet.run(ev(thiz))

    def downCast[U2 >: U] = thiz.asInstanceOf[Computation[A, U2]]
  
    /** Simplifies effectful creation of handlers (handlers that depend on other effects).
     * 
     *  Passes computed value to handler constructor.
     *  Effect used to compute the value, are absorbed as handler's dependencies.
     */ 
    def >>=![F[+_], L, N](f: A => Handler[F, L, N]): Handler[F, L, U & N] = Handler.flatHandle(thiz.map(f))

    /** Applies a handler to this computation.
     *
     *  Same as `myHandler.handle(this)`.
     */
    def handleWith[V]: HandleWithApply[A, U, V] = new HandleWithApply[A, U, V](thiz)


  extension [F[+_], L, N](thiz: Computation[Handler[F, L, N], N])
    /** Simplifies effectful creation of handlers (handlers that depend on other effects).
     * 
     *  Same as `Handler.flatHandle(this)`.
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

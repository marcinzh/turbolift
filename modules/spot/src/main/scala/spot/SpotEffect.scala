package spot
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import cats.effect.{Async}
import cats.effect.kernel.{Deferred, Ref, Cont, Poll, Sync, Fiber => CatsFiber}
import turbolift.!!
import turbolift.effects.IO
import spot.classes.TheMonad
import spot.internals.{SpotRef, SpotDeferred, SpotExec, SpotFiber}


object SpotEffect:
  given theAsync[U <: IO]: Async[!![_, U]] = new TheMonad[U] with Async[!![_, U]]:
    type TMonad[A] = A !! U
    type TRef[A] = Ref[TMonad, A]
    type TDeferred[A] = Deferred[TMonad, A]
    type TFiber[A] = CatsFiber[TMonad, Throwable, A]
    type TPoll = Poll[TMonad]
    type TCont[K, R] = Cont[TMonad, K, R]

    //// Members declared in cats.ApplicativeError

    override def handleErrorWith[A](fa: A !! U)(f: Throwable => A !! U): A !! U = IO.catchAllEff(fa)(f)

    override def raiseError[A](e: Throwable): A !! U = IO.raise(e)

    //// Members declared in cats.effect.kernel.GenConcurrent

    override def deferred[A]: TDeferred[A] !! U = SpotDeferred.create()

    override def ref[A](a: A): TRef[A] !! U = SpotRef.create(a)

    //// Members declared in cats.effect.kernel.Async

    override def cont[K, R](body: TCont[K, R]): R !! U = Async.defaultCont(body)(using this)

    override def evalOn[A](fa: A !! U, ec: ExecutionContext): A !! U = SpotExec.evalOn(fa, ec)

    override def executionContext: ExecutionContext !! U = SpotExec.current

    //// Members declared in cats.effect.kernel.Clock

    override def monotonic: FiniteDuration !! U = IO.nanoTime

    override def realTime: FiniteDuration !! U = IO.now

    //// Members declared in cats.effect.kernel.GenSpawn

    override def cede: Unit !! U = IO.yeld

    override def start[A](fa: A !! U): TFiber[A] !! U = SpotFiber.start(fa)

    //// Members declared in cats.effect.kernel.GenTemporal

    protected def sleep(time: FiniteDuration): Unit !! U = IO.sleep(time)

    //// Members declared in cats.effect.kernel.MonadCancel
    
    override def canceled: Unit !! U = IO.cancel

    override def forceR[A, B](fa: A !! U)(fb: B !! U): B !! U = IO.catchAll(fa.void)(_ => !!.unit) &&! fb

    override def onCancel[A](fa: A !! U, fin: Unit !! U): A !! U = IO.onCancel(fa)(fin)

    override def uncancelable[A](body: TPoll => A !! U): A !! U =
      IO.uncancellableWith: restore =>
        val poll = new TPoll:
          override def apply[X](aa: X !! U) = restore(aa)
        body(poll)

    //// Members declared in cats.effect.kernel.Sync

    override def suspend[A](hint: Sync.Type)(thunk: => A): A !! U =
      hint match
        case Sync.Type.Delay => IO(thunk)
        case Sync.Type.Blocking |
             Sync.Type.InterruptibleOnce |
             Sync.Type.InterruptibleMany => IO.blocking(thunk)

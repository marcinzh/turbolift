package turbolift.effects
import java.time.Instant
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import scala.concurrent.duration._
import turbolift.{!!, Signature, ComputationCases => CC}
import turbolift.io.{Cause, Snap}
import turbolift.interpreter.Prompt
import turbolift.internals.executor.Executor


sealed trait IO extends Signature


/** The sole instance of `IO` effect.
  *
  * Unlike other effects, it cannot be interpreted/handled by the user.
  * Once introduced into computation, it stays there forever.
  * That is, until `unsafeRun`.
  */

case object IO extends IO:
  final override type ThisEffect = IO


  //---------- Side Effects ----------


  //** Alias of [[sync]] */
  def apply[A](thunk: => A): A !! IO = sync(thunk)

  def sync[A](thunk: => A): A !! IO = CC.sync(isAttempt = false, thunk)

  def async[A](callback: (Either[Throwable, A] => Unit) => Unit): A !! IO = CC.intrinsic(_.intrinsicAsync(callback, isAttempt = false))

  def blocking[A](thunk: => A): A !! IO = CC.intrinsic(_.intrinsicBlocking(() => thunk, isAttempt = false))

  def attemptSync[A](thunk: => A): Either[Throwable, A] !! IO = CC.sync(isAttempt = true, thunk)

  def attemptAsync[A](callback: (Either[Throwable, A] => Unit) => Unit): Either[Throwable, A] !! IO = CC.intrinsic(_.intrinsicAsync(callback, isAttempt = true))

  def attemptBlocking[A](thunk: => A): Either[Throwable, A] !! IO = CC.intrinsic(_.intrinsicBlocking(() => thunk, isAttempt = true))

  def attemptEff[A, U <: IO](body: A !! U): Either[Throwable, A] !! U =
    snap(body).flatMap:
      case Snap.Success(a) => !!.pure(Right(a))
      case Snap.Failure(c) => !!.pure(c.toEither)
      case aa: Snap.NotSuccess => unsnap(aa)

  @deprecated("Use attemptBlocking")
  def blockingAttempt[A](thunk: => A): Either[Throwable, A] !! IO = CC.intrinsic(_.intrinsicBlocking(() => thunk, isAttempt = true))


  //---------- Exceptions ----------


  def fail(c: Cause): Nothing !! IO = unsnap(Snap.Failure(c))

  def raise(e: Throwable): Nothing !! IO = fail(Cause.Thrown(e))

  def raiseFromOption[A](ee: Option[A])(e: => Throwable): A !! IO = ee.fold(raise(e))(!!.pure)

  def raiseFromEither[A](ee: Either[Throwable, A]): A !! IO = ee.fold(raise, !!.pure)

  def raiseFromTry[A](ee: Try[A]): A !! IO = ee.fold(raise, !!.pure)

  def catchToOption[A, U <: IO](body: A !! U): Option[A] !! U = catchToTry(body).map(_.toOption)

  def catchToEither[A, U <: IO](body: A !! U): Either[Throwable, A] !! U = catchToTry(body).map(_.toEither)

  def catchToTry[A, U <: IO](body: A !! U): Try[A] !! U =
    snap(body).flatMap:
      case Snap.Success(a) => !!.pure(TrySuccess(a)) 
      case Snap.Failure(c) => !!.pure(c.toTry) 
      case aa: Snap.NotSuccess => unsnap(aa)

  def catchAll[A, U <: IO](body: A !! U)(f: Throwable => A): A !! U = catchAllEff(body)(f.andThen(!!.pure))

  def catchAllEff[A, U <: IO](body: A !! U)(f: Throwable => A !! U): A !! U =
    snap(body).flatMap:
      case Snap.Failure(e) => f(e.last)
      case aa => unsnap(aa)

  def catchSome[A, U <: IO](body: A !! U)(f: PartialFunction[Throwable, A]): A !! U = catchSomeEff(body)(f.andThen(!!.pure))

  def catchSomeEff[A, U <: IO](body: A !! U)(f: PartialFunction[Throwable, A !! U]): A !! U =
    catchAllEff(body)(f.applyOrElse(_, raise))


  @deprecated("Use raiseFromOption") def fromOption[A](ee: Option[A])(e: => Throwable): A !! IO = raiseFromOption(ee)(e)
  @deprecated("Use raiseFromEither") def fromEither[A](ee: Either[Throwable, A]): A !! IO = raiseFromEither(ee)
  @deprecated("Use raiseFromTry") def fromTry[A](ee: Try[A]): A !! IO = raiseFromTry(ee)
  @deprecated("Use catchToOption") def toOption[A, U <: IO](body: A !! U): Option[A] !! U = catchToOption(body)
  @deprecated("Use catchToEither") def toEither[A, U <: IO](body: A !! U): Either[Throwable, A] !! U = catchToEither(body)
  @deprecated("Use catchToTry") def toTry[A, U <: IO](body: A !! U): Try[A] !! U = catchToTry(body)


  //---------- Finalization ----------


  def guarantee[A, U <: IO](release: Unit !! U)(body: A !! U): A !! U = UnsafeIO.guarantee(release)(body)

  def guaranteeSnap[A, U <: IO](release: Snap[A] => A !! U)(body: A !! U): A !! U = UnsafeIO.guaranteeSnap(release)(body)

  def bracket[A, B, U <: IO](acquire: A !! U, release: A => Unit !! U)(use: A => B !! U): B !! U = UnsafeIO.bracket(acquire, release)(use)

  def bracketVoid[A, U <: IO](acquire: Unit !! U, release: Unit !! U)(use: A !! U): A !! U = UnsafeIO.bracketVoid(acquire, release)(use)

  def bracketSnap[A, B, U <: IO](acquire: A !! U, release: (A, Snap[B]) => B !! U)(use: A => B !! U): B !! U = UnsafeIO.bracketSnap(acquire, release)(use)

  def cancellable[A, U <: IO](comp: A !! U): A !! U = UnsafeIO.cancellable(comp)

  def cancellableSnap[A, U <: IO](comp: A !! U): Snap[A] !! U = UnsafeIO.cancellableSnap(comp)

  def uncancellable[A, U <: IO](comp: A !! U): A !! U = UnsafeIO.uncancellable(comp)

  def snap[A, U <: IO](body: A !! U): Snap[A] !! U = UnsafeIO.snap(body)

  def unsnap[A](aa: Snap[A]): A !! IO = UnsafeIO.unsnap(aa)

  def onFailure[A, U <: IO](body: A !! U)(f: Throwable => Unit !! U): A !! U = UnsafeIO.onFailure(body)(f)

  def onCancel[A, U <: IO](body: A !! U)(comp: Unit !! U): A !! U = UnsafeIO.onCancel(body)(comp)


  //---------- Time ----------


  def sleep(millis: Long): Unit !! IO = CC.intrinsic(_.intrinsicSleep(millis))

  def sleep(duration: FiniteDuration): Unit !! IO = CC.intrinsic(_.intrinsicSleep(duration.length, duration.unit))

  def delay[A, U <: IO](millis: Long)(comp: A !! U): A !! U = sleep(millis) &&! comp

  def delay[A, U <: IO](duration: FiniteDuration)(comp: A !! U): A !! U = sleep(duration) &&! comp

  val nowRaw: Long !! IO = !!.impure(System.currentTimeMillis())

  val now: FiniteDuration !! IO = !!.impure(new FiniteDuration(System.currentTimeMillis(), MILLISECONDS))

  val nanoTimeRaw: Long !! IO = !!.impure(System.nanoTime())

  val nanoTime: FiniteDuration !! IO = !!.impure(new FiniteDuration(System.nanoTime(), NANOSECONDS))

  def instant: Instant !! IO = !!.impure(Instant.now().nn)

  def timed[A, U <: IO](comp: A !! U): (A, FiniteDuration) !! U =
    for
      t0 <- !!.impure(System.nanoTime())
      a <- comp
      t1 = System.nanoTime()
      d = new FiniteDuration(t1 - t0, NANOSECONDS)
    yield (a, d)


  //---------- Others ----------


  def executor: Executor !! IO = CC.intrinsic(_.intrinsicEnvAsk(_.executor))

  def executeOn[A, U <: IO](exec: Executor)(body: A !! U): A !! U = CC.intrinsic(_.intrinsicExecOn(exec, body))

  val cancel: Nothing !! IO = unsnap(Snap.Cancelled)

  val yeld: Unit !! IO = CC.intrinsic(_.intrinsicYield)


/** Some [[IO]] operations with [[IO]] effect removed from the signature.
 *
 * Provided because in some circumstances they may be safely considered as [[IO]]-free.
 */
object UnsafeIO:
  def guarantee[A, U](release: Unit !! U)(body: A !! U): A !! U =
    guaranteeSnap[A, U](aa => release &&! unsnap(aa))(body)

  def guaranteeSnap[A, U](release: Snap[A] => A !! U)(body: A !! U): A !! U =
    uncancellable:
      cancellableSnap(body)
      .flatMap(release)

  def bracket[A, B, U](acquire: A !! U, release: A => Unit !! U)(use: A => B !! U): B !! U =
    bracketSnap[A, B, U](acquire, (a, bb) => release(a) &&! unsnap(bb))(use)

  def bracketSnap[A, B, U](acquire: A !! U, release: (A, Snap[B]) => B !! U)(use: A => B !! U): B !! U =
    uncancellable:
      acquire.flatMap: a =>
        cancellableSnap(use(a))
        .flatMap(release(a, _))

  def bracketVoid[A, U](acquire: Unit !! U, release: Unit !! U)(use: A !! U): A !! U =
    bracket(acquire, _ => release)(_ => use)

  def cancellable[A, U](comp: A !! U): A !! U = CC.intrinsic(_.intrinsicSuppress(comp, -1))

  def uncancellable[A, U](comp: A !! U): A !! U = CC.intrinsic(_.intrinsicSuppress(comp, +1))

  //@#@TODO fuse
  def cancellableSnap[A, U](comp: A !! U): Snap[A] !! U = snap(cancellable(comp))

  def snap[A, U](body: A !! U): Snap[A] !! U = CC.intrinsic(_.intrinsicSnap(body))

  def unsnap[A](aa: Snap[A]): A !! Any = CC.intrinsic(_.intrinsicUnsnap(aa))

  def onSnapSome[A, U](body: A !! U)(f: PartialFunction[Snap[A], Unit !! U]) = 
    snap(body).flatMap(ss => f.lift(ss).getOrElse(!!.unit) &&! unsnap(ss))

  def onAbort[A, U](body: A !! U)(f: (Any, Prompt) => Unit !! U): A !! U =
    //@#@WTF unreachable case
    // onSnapSome(body) { case Snap.Aborted(x, p) => f(x, p) }
    onSnapSome(body) { case ss: Snap.Aborted => f(ss.value, ss.prompt) }

  def onFailure[A, U](body: A !! U)(f: Throwable => Unit !! U): A !! U =
    onSnapSome(body) { case Snap.Failure(c) => f(c.last) }

  def onCancel[A, U](body: A !! U)(comp: Unit !! U): A !! U =
    onSnapSome(body) { case Snap.Cancelled => comp }

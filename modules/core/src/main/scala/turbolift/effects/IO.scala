package turbolift.effects
import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import scala.concurrent.duration._
import turbolift.{!!, Signature, ComputationCases => CC}
import turbolift.data.{Cause, Snap}
import turbolift.io.{Zipper, Fiber, Warp, OnceVar}
import turbolift.interpreter.Prompt
import turbolift.internals.executor.Executor
import turbolift.io.Zipper
import turbolift.Extensions._


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
      case Snap.Failure(Cause.Thrown(e), _) => succeed(Left(e), None)
      case Snap.Failure(c, s) => fail(c, s)

  @deprecated("Use attemptBlocking")
  def blockingAttempt[A](thunk: => A): Either[Throwable, A] !! IO = CC.intrinsic(_.intrinsicBlocking(() => thunk, isAttempt = true))


  //---------- Exceptions ----------


  /** Type-unsafe because of lack `IO` in the result type */

  //// experimental
  def succeed[A](value: A, suppressed: Option[Cause]): A !! Any = CC.intrinsic(_.intrinsicSucceed(value, suppressed, reset = true))

  //// experimental
  def fail(cause: Cause.Singular, suppressed: Option[Cause]): Nothing !! Any = failExt(cause, suppressed, reset = true)

  //// experimental
  def failFromOutcome(cause: Cause): Nothing !! Any = { val (c, s) = cause.split; IO.failExt(c, s, reset = false) }

  //// experimental
  private def failExt(cause: Cause.Singular, suppressed: Option[Cause], reset: Boolean): Nothing !! Any = CC.intrinsic(_.intrinsicFail(cause, suppressed, reset))

  def raise(e: Throwable): Nothing !! IO = failExt(Cause.Thrown(e), None, reset = false)

  def raiseFromOption[A](ee: Option[A])(e: => Throwable): A !! IO = ee.fold(raise(e))(!!.pure)

  def raiseFromEither[A](ee: Either[Throwable, A]): A !! IO = ee.fold(raise, !!.pure)

  def raiseFromTry[A](ee: Try[A]): A !! IO = ee.fold(raise, !!.pure)

  def catchToOption[A, U <: IO](body: A !! U): Option[A] !! U = catchToTry(body).map(_.toOption)

  def catchToEither[A, U <: IO](body: A !! U): Either[Throwable, A] !! U = catchToTry(body).map(_.toEither)

  def catchToTry[A, U <: IO](body: A !! U): Try[A] !! U =
    snap(body).flatMap:
      case Snap.Success(a) => !!.pure(TrySuccess(a))
      case Snap.Failure(Cause.Thrown(e), _) => succeed(TryFailure(e), None)
      case Snap.Failure(c, s) => fail(c, s)

  def catchAll[A, U <: IO](body: A !! U)(f: Throwable => A): A !! U = catchAllEff(body)(f.andThen(!!.pure))

  def catchAllEff[A, U <: IO](body: A !! U)(f: Throwable => A !! U): A !! U =
    snap(body).flatMap:
      case Snap.Success(a) => !!.pure(a)
      case Snap.Failure(Cause.Thrown(e), _) => f(e).flatMap(succeed(_, None))
      case Snap.Failure(c, s) => fail(c, s)

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


  //// Functions defined in this section don't introduce `IO` to the result, despite being defined
  //// in the `IO` module. It was decided so, because finalizers may run effects not polluted by `IO`.


  def guarantee[A, U](release: Unit !! U)(body: A !! U): A !! U =
    uncancellableWith: restore =>
      restore.snap(body).flatMap: s =>
        release &&! unsnap(s)

  def guaranteeSnap[A, U](release: Snap[A] => Unit !! U)(body: A !! U): A !! U =
    uncancellableWith: restore =>
      restore.snap(body).flatMap: s =>
        release(s) &&! unsnap(s)

  def bracket[A, B, U](acquire: A !! U, release: A => Unit !! U)(use: A => B !! U): B !! U =
    bracketSnap[A, B, U](acquire, (a, bb) => release(a) &&! unsnap(bb))(use)

  def bracket[A, U](acquire: Unit !! U, release: Unit !! U)(use: A !! U): A !! U =
    bracket(acquire, _ => release)(_ => use)

  def bracketSnap[A, B, U](acquire: A !! U, release: (A, Snap[B]) => B !! U)(use: A => B !! U): B !! U =
    uncancellableWith: restore =>
      acquire.flatMap: a =>
        restore.snap(use(a))
        .flatMap(release(a, _))

  def isCancellable: Boolean !! Any = !!.envAsk(_.isCancellable)

  def cancellable[A, U](comp: A !! U): A !! U = CC.intrinsic(_.intrinsicSuppress(true, _ => comp))

  //@#@TODO fuse
  def cancellableSnap[A, U](comp: A !! U): Snap[A] !! U = snap(cancellable(comp))

  def uncancellable[A, U](comp: A !! U): A !! U = CC.intrinsic(_.intrinsicSuppress(false, _ => comp))

  def uncancellableWith[A, U](body: RestoreCancellable => A !! U): A !! U =
    CC.intrinsic(_.intrinsicSuppress(false, x => body(RestoreCancellable(x))))

  final class RestoreCancellable(val wasCancellable: Boolean):
    def apply[A, U](body: A !! U): A !! U = if wasCancellable then cancellable(body) else body
    def snap[A, U](body: A !! U): Snap[A] !! U = if wasCancellable then cancellableSnap(body) else IO.snap(body)

  def snap[A, U](body: A !! U): Snap[A] !! U = CC.intrinsic(_.intrinsicSnap(body))

  def unsnap[A](snap: Snap[A]): A !! Any =
    snap match
      case Snap.Success(a) => !!.pure(a)
      case Snap.Failure(c, s) => fail(c, s)

  def onSuccess[A, U](body: A !! U)(f: A => Unit !! U): A !! U =
    snap(body).flatMap:
      case Snap.Success(a) => f(a).as(a)
      case Snap.Failure(c, s) => fail(c, s)

  def onFailure[A, U](body: A !! U)(f: Cause => Unit !! U): A !! U =
    snap(body).flatMap:
      case Snap.Success(a) => !!.pure(a)
      case Snap.Failure(c, s) => f(c) &&! fail(c, s)

  def onSomeFailure[A, U](body: A !! U)(f: PartialFunction[Cause, Unit !! U]) =
    snap(body).flatMap:
      case Snap.Success(a) => !!.pure(a)
      case Snap.Failure(c, s) => f.lift(c).getOrElse(!!.unit) &&! fail(c, s)

  def onAbort[A, U](body: A !! U)(f: (Any, Prompt) => Unit !! U): A !! U =
    onSomeFailure(body) { case c: Cause.Aborted => f(c.value, c.prompt) }

  def onException[A, U](body: A !! U)(f: Throwable => Unit !! U): A !! U =
    onSomeFailure(body) { case Cause.Thrown(e) => f(e) }

  def onCancel[A, U](body: A !! U)(comp: => Unit !! U): A !! U =
    onSomeFailure(body) { case Cause.Cancelled => comp }


  //---------- Time ----------


  def sleep(millis: Long): Unit !! IO = CC.intrinsic(_.intrinsicSleep(millis, TimeUnit.MILLISECONDS))

  def sleep(duration: FiniteDuration): Unit !! IO = CC.intrinsic(_.intrinsicSleep(duration.length, duration.unit))

  def delay[A, U <: IO](comp: A !! U, millis: Long): A !! U = sleep(millis) &&! comp

  def delay[A, U <: IO](comp: A !! U, duration: FiniteDuration): A !! U = sleep(duration) &&! comp

  def timeout[A, U <: IO](comp: A !! U, millis: Long): Option[A] !! U =
    CC.intrinsic(_.intrinsicRaceSleep(comp, millis, TimeUnit.MILLISECONDS))

  def timeout[A, U <: IO](comp: A !! U, duration: FiniteDuration): Option[A] !! U =
    CC.intrinsic(_.intrinsicRaceSleep(comp, duration.length, duration.unit))

  def timeoutTo[A, U <: IO](comp: A !! U, millis: Long, fallback: => A): A !! U =
    timeout(comp, millis).map(_.getOrElse(fallback))

  def timeoutTo[A, U <: IO](comp: A !! U, duration: FiniteDuration, fallback: => A): A !! U =
    timeout(comp, duration).map(_.getOrElse(fallback))

  def timeoutToEff[A, U <: IO](comp: A !! U, millis: Long, fallback: => A !! U): A !! U =
    timeout(comp, millis).flatMap(_.fold(fallback)(!!.pure))

  def timeoutToEff[A, U <: IO](comp: A !! U, duration: FiniteDuration, fallback: => A !! U): A !! U =
    timeout(comp, duration).flatMap(_.fold(fallback)(!!.pure))

  val nowRaw: Long !! IO = !!.impure(System.currentTimeMillis())

  val now: FiniteDuration !! IO = !!.impure(new FiniteDuration(System.currentTimeMillis(), MILLISECONDS))

  val nanoTimeRaw: Long !! IO = !!.impure(System.nanoTime())

  val nanoTime: FiniteDuration !! IO = !!.impure(new FiniteDuration(System.nanoTime(), NANOSECONDS))

  def instant: Instant !! IO = !!.impure[Instant](Instant.now().nn)

  def timed[A, U <: IO](comp: A !! U): (A, FiniteDuration) !! U =
    for
      t0 <- !!.impure(System.nanoTime())
      a <- comp
      t1 = System.nanoTime()
      d = new FiniteDuration(t1 - t0, NANOSECONDS)
    yield (a, d)


  //---------- Race ----------


  def race[A, B, U <: IO](lhs: A !! U, rhs: A !! U): A !! U = raceEither(lhs, rhs).map(_.merge)

  def raceEither[A, B, U <: IO](lhs: A !! U, rhs: B !! U): Either[A, B] !! U = CC.intrinsic(_.intrinsicRaceEither(lhs, rhs))

  def raceBoth[A, B, U](lhs: A !! U, rhs: B !! U): (A, B) !! U = raceBothWith(lhs, rhs)(pairCtorFun[A, B])

  def raceBothWith[A, B, C, U](lhs: A !! U, rhs: B !! U)(f: (A, B) => C): C !! U = CC.intrinsic(_.intrinsicRaceBothWith(lhs, rhs, f))

  def raceFirst[A, U <: IO](comps: Iterable[A !! U]): A !! U = CC.intrinsic(_.intrinsicRaceFirst(comps))

  def raceAll[A, U](comps: Iterable[A !! U]): Vector[A] !! U = CC.intrinsic(_.intrinsicRaceAll(comps, isVoid = false))

  def raceAllVoid[U](comps: Iterable[Unit !! U]): Unit !! U = CC.intrinsic(_.intrinsicRaceAll(comps, isVoid = true))

  def raceOne[A, U <: IO](comp: A !! U): Option[A] !! U = CC.intrinsic(_.intrinsicRaceOne(comp))

  def orElse[A, U <: IO](lhs: A !! U, rhs: => A !! U): A !! U =
    IO.raceOne(lhs).flatMap:
      case Some(a) => !!.pure(a)
      case None => rhs

  def either[A, B, U <: IO](lhs: A !! U, rhs: => B !! U): Either[A, B] !! U =
    IO.raceOne(lhs).flatMap:
      case Some(a) => !!.pure(Left(a))
      case None => rhs.map(Right(_))

  def raceFibers[A, B, U, V](comp1: A !! U, comp2: B !! V): Either[(Zipper[A, U], Fiber[B, V]), (Fiber[A, U], Zipper[B, V])] !! (IO & Warp) =
    for
      ovar <- OnceVar.create[Either[(Zipper[A, U], Fiber[B, V]), (Fiber[A, U], Zipper[B, V])]]
      fib1 <- Fiber.forkIdle[A, U](null, "Left")
      fib2 <- Fiber.forkIdle[B, V](null, "Right")
      _ = fib1.unsafeStart(comp1, zipp => ovar.unsafePut(Left((zipp, fib2))))
      _ = fib2.unsafeStart(comp2, zipp => ovar.unsafePut(Right((fib1, zipp))))
      x <- ovar.get
    yield x

  private def pairCtorFun[A, B]: (A, B) => (A, B) = pairCtorVal.asInstanceOf[(A, B) => (A, B)]
  private val pairCtorVal: (Any, Any) => Any = (_, _)


  //---------- Others ----------


  def executor: Executor !! IO = CC.intrinsic(_.intrinsicEnvAsk(_.executor))

  def executeOn[A, U <: IO](exec: Executor)(body: A !! U): A !! U = CC.intrinsic(_.intrinsicExecOn(exec, body))

  val cancel: Nothing !! IO = CC.intrinsic(_.intrinsicSelfCancel)

  val yeld: Unit !! IO = CC.intrinsic(_.intrinsicYield)


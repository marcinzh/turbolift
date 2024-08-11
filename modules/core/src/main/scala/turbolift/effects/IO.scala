package turbolift.effects
import java.time.Instant
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import scala.concurrent.duration._
import turbolift.{!!, Signature}
import turbolift.io.{Cause, Snap}
import turbolift.internals.executor.Executor
import turbolift.internals.primitives.{ComputationCases => CC}


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


  def apply[A](value: => A): A !! IO = CC.intristic(_.intristicDoIO(() => value, isAttempt = false))

  def attempt[A](value: => A): Either[Throwable, A] !! IO = CC.intristic(_.intristicDoIO(() => value, isAttempt = true))

  def blocking[A](value: => A): A !! IO = CC.intristic(_.intristicBlocking(() => value, isAttempt = false))
  
  def blockingAttempt[A](value: => A): Either[Throwable, A] !! IO = CC.intristic(_.intristicBlocking(() => value, isAttempt = true))


  //---------- Exceptions ----------


  def raise(e: Throwable): Nothing !! IO = fail(Cause.Thrown(e))

  def fail(c: Cause): Nothing !! IO = unsnap(Snap.Failure(c))

  def fromOption[A](ee: Option[A])(e: => Throwable): A !! IO = ee.fold(raise(e))(!!.pure)

  def fromEither[A](ee: Either[Throwable, A]): A !! IO = ee.fold(raise, !!.pure)

  def fromTry[A](ee: Try[A]): A !! IO = ee.fold(raise, !!.pure)

  def toOption[A, U <: IO](body: A !! U): Option[A] !! U = toTry(body).map(_.toOption)

  def toEither[A, U <: IO](body: A !! U): Either[Throwable, A] !! U = toTry(body).map(_.toEither)

  def toTry[A, U <: IO](body: A !! U): Try[A] !! U =
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

  def onFailure[A, U <: IO](body: A !! U)(f: Throwable => Unit !! U): A !! U =
    snap(body).flatMap:
      case ss @ Snap.Failure(c) => f(c.last) &&! unsnap(ss)
      case aa => unsnap(aa)

  def onCancel[A, U <: IO](body: A !! U)(comp: Unit !! U): A !! U =
    snap(body).flatMap:
      case ss @ Snap.Cancelled => comp &&! unsnap(ss)
      case aa => unsnap(aa)


  //---------- Finalization ----------


  def guarantee[A, U <: IO](release: Unit !! U)(body: A !! U): A !! U =
    guaranteeSnap[A, U](aa => release &&! unsnap(aa))(body)

  def guaranteeSnap[A, U <: IO](release: Snap[A] => A !! U)(body: A !! U): A !! U =
    uncancellable:
      cancellableSnap(body)
      .flatMap(release)

  def bracket[A, B, U <: IO](acquire: A !! U)(release: A => Unit !! U)(use: A => B !! U): B !! U =
    bracketSnap[A, B, U](acquire)((a, bb) => release(a) &&! unsnap(bb))(use)

  def bracketSnap[A, B, U <: IO](acquire: A !! U)(release: (A, Snap[B]) => B !! U)(use: A => B !! U): B !! U =
    uncancellable:
      acquire.flatMap: a =>
        cancellableSnap(use(a))
        .flatMap(release(a, _))

  def cancellable[A, U <: IO](comp: A !! U): A !! U = CC.intristic(_.intristicSuppress(comp, -1))

  //@#@TODO fuse
  def cancellableSnap[A, U <: IO](comp: A !! U): Snap[A] !! U = snap(cancellable(comp))

  def uncancellable[A, U <: IO](comp: A !! U): A !! U = CC.intristic(_.intristicSuppress(comp, +1))

  def snap[A, U <: IO](body: A !! U): Snap[A] !! U = CC.intristic(_.intristicSnap(body))

  def unsnap[A](aa: Snap[A]): A !! IO = CC.intristic(_.intristicUnsnap(aa))


  //---------- Time ----------


  def sleep(millis: Long): Unit !! IO = CC.intristic(_.intristicSleep(millis))

  def sleep(duration: FiniteDuration): Unit !! IO = CC.intristic(_.intristicSleep(duration.length, duration.unit))

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


  def executor: Executor !! IO = CC.intristic(_.intristicEnvAsk(_.executor))

  def executeOn[A, U <: IO](exec: Executor)(body: A !! U): A !! U = CC.intristic(_.intristicExecOn(exec, body))

  val cancel: Nothing !! IO = unsnap(Snap.Cancelled)

  val yeld: Unit !! IO = CC.intristic(_.intristicYield)

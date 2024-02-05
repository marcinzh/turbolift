package turbolift.effects
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import turbolift.{!!, Signature}
import turbolift.io.{Cause, Snap}
import turbolift.internals.primitives.Primitives
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

  def apply[A](value: => A): A !! IO = CC.Try(() => value)

  def cancel: Nothing !! IO = Primitives.unsnap(Snap.Cancelled)

  def yeld: Unit !! IO = Primitives.yeld

  def blocking[A](value: => A): A !! IO =
    //@#@TODO
    apply(value)


  //---------- Exceptions ----------


  def raise(e: Throwable): Nothing !! IO = fail(Cause.Thrown(e))

  def fail(c: Cause): Nothing !! IO = unsnap(Snap.Failure(c))

  def fromOption[A](ee: Option[A])(e: => Throwable): A !! IO = ee.fold(raise(e))(!!.pure)

  def fromEither[A](ee: Either[Throwable, A]): A !! IO = ee.fold(raise, !!.pure)

  def fromTry[A](ee: Try[A]): A !! IO = ee.fold(raise, !!.pure)

  def toOption[A, U <: IO](body: A !! U): Option[A] !! U = toTry(body).map(_.toOption)

  def toEither[A, U <: IO](body: A !! U): Either[Throwable, A] !! U = toTry(body).map(_.toEither)

  def toTry[A, U <: IO](body: A !! U): Try[A] !! U =
    snap(body):
      case Snap.Success(a) => !!.pure(TrySuccess(a)) 
      case Snap.Failure(c) => !!.pure(c.toTry) 
      case aa: Snap.NotSuccess => unsnap(aa)

  def catchAll[A, U <: IO](body: A !! U)(f: Throwable => A): A !! U = catchAllEff(body)(f.andThen(!!.pure))

  def catchAllEff[A, U <: IO](body: A !! U)(f: Throwable => A !! U): A !! U =
    snap(body):
      case Snap.Failure(e) => f(e.last)
      case aa => unsnap(aa)

  def catchSome[A, U <: IO](body: A !! U)(f: PartialFunction[Throwable, A]): A !! U = catchSomeEff(body)(f.andThen(!!.pure))

  def catchSomeEff[A, U <: IO](body: A !! U)(f: PartialFunction[Throwable, A !! U]): A !! U =
    catchAllEff(body)(f.applyOrElse(_, raise))

  def onFailure[A, U <: IO](body: A !! U)(f: Throwable => Unit !! U): A !! U =
    snap(body):
      case cc @ Snap.Failure(c) => f(c.last) &&! unsnap(cc)
      case aa => unsnap(aa)


  //---------- Snap ----------


  def unsnap[A](aa: Snap[A]): A !! IO = Primitives.unsnap(aa)

  def snap[A, B, U <: IO](body: A !! U)(f: Snap[A] => B !! U): B !! U = Primitives.snap(body)(f)


  //---------- Resource ----------


  def guarantee[A, U](release: Unit !! U)(body: A !! U): A !! U =
    Primitives.snap(body)(aa => release &&! Primitives.unsnap(aa))

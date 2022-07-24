package turbolift.internals.engine
import scala.annotation.tailrec
import turbolift.typeclass.MonadZip
import TrampolineCases._


private[engine] sealed trait Trampoline[A]:
  final def map[B](f: A => B): Trampoline[B] = flatMap(a => Done(f(a)))

  final def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    this match
      case FlatMap(that, g) => FlatMap(that, x => g(x).flatMap(f))
      case x: DoneOrMore[A] => FlatMap(x, f)

  final def zip[B](that: Trampoline[B]): Trampoline[(A, B)] =
    (resume, that.resume) match
      case (Right(a), Right(b)) => Done((a, b))
      case (Right(a), Left(fb)) => More(() => fb().zipR(a))
      case (Left(fa), Right(b)) => More(() => fa().zipL(b))
      case (Left(fa), Left(fb)) => More(() => fa().zip(fb()))

  final def zipL[B](b: B): Trampoline[(A, B)] =
    resume match
      case Right(a) => Done((a, b))
      case Left(fa) => More(() => fa().zipL(b))

  final def zipR[B](b: B): Trampoline[(B, A)] =
    resume match
      case Right(a) => Done((b, a))
      case Left(fa) => More(() => fa().zipR(b))

  @tailrec final def run: A =
    resume match
      case Right(a) => a
      case Left(th) => th().run

  @tailrec final def resume: Either[() => Trampoline[A], A] =
    this match
      case Done(a) => Right(a)
      case More(th) => Left(th)
      case FlatMap(that, f) => that match
        case Done(b) => f(b).resume
        case More(th) => Left(() => th().flatMap(f))


private[engine] object TrampolineCases:
  sealed trait DoneOrMore[A] extends Trampoline[A]
  final case class Done[A](value: A) extends DoneOrMore[A]
  final case class More[A](thunk: () => Trampoline[A]) extends DoneOrMore[A]
  final case class FlatMap[A, B](that: DoneOrMore[A], kont: A => Trampoline[B]) extends Trampoline[B]


private[engine] object TrampolineInstances:
  def monad: MonadZip[Trampoline] = new MonadZip[Trampoline]:
    override def pure[A](a: A): Trampoline[A] = Done(a)
    override def flatMap[A, B](ma: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = ma.flatMap(f)
    override def zip[A, B](ma: Trampoline[A], mb: Trampoline[B]): Trampoline[(A, B)] = ma.zip(mb)

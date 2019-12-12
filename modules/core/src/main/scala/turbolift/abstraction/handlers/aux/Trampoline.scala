package turbolift.abstraction.handlers.aux
import mwords._
import scala.annotation.tailrec
import TrampolineCases._


sealed trait Trampoline[+A] {
  final def map[B](f: A => B): Trampoline[B] = flatMap(a => Done(f(a)))

  final def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    this match {
      case FlatMap(that, g) => FlatMap(that, (x: Any) => g(x).flatMap(f))
      case x: DoneOrMore[A] => FlatMap(x, f)
    }

  final def zipPar[B](that: Trampoline[B]): Trampoline[(A, B)] =
    (resume, that.resume) match {
      case (Right(a), Right(b)) => Done((a, b))
      case (Right(a), Left(fb)) => More(() => fb().zipParR(a))
      case (Left(fa), Right(b)) => More(() => fa().zipParL(b))
      case (Left(fa), Left(fb)) => More(() => fa().zipPar(fb()))
    }

  final def zipParL[B](b: B): Trampoline[(A, B)] =
    resume match {
      case Right(a) => Done((a, b))
      case Left(fa) => More(() => fa().zipParL(b))
    }

  final def zipParR[B](b: B): Trampoline[(B, A)] =
    resume match {
      case Right(a) => Done((b, a))
      case Left(fa) => More(() => fa().zipParR(b))
    }

  @tailrec final def run: A =
    resume match {
      case Right(a) => a
      case Left(th) => th().run
    }

  @tailrec final def resume: Either[() => Trampoline[A], A] =
    this match {
      case Done(a) => Right(a)
      case More(th) => Left(th)
      case FlatMap(that, f) => that match {
        case Done(b) => f(b).resume
        case More(th) => Left(() => th().flatMap(f))
      }
    }
}


object TrampolineCases {
  sealed trait DoneOrMore[A] extends Trampoline[A]
  case class Done[A](value: A) extends DoneOrMore[A]
  case class More[A](thunk: () => Trampoline[A]) extends DoneOrMore[A]
  case class FlatMap[A, +B](that: DoneOrMore[A], kont: A => Trampoline[B]) extends Trampoline[B]
}


object Trampoline {
  import TrampolineCases._

  def done[A](a: A): Trampoline[A] = Done(a)
  def defer[A](a : => A): Trampoline[A] = More(() => Done(a))
}


object TrampolineInstances {
  def monad: MonadPar[Trampoline] = new MonadPar[Trampoline] {
    def pure[A](a: A): Trampoline[A] = Done(a)
    def flatMap[A, B](ma: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = ma.flatMap(f)
    def zipPar[A, B](ma: Trampoline[A], mb: Trampoline[B]): Trampoline[(A, B)] = ma.zipPar(mb)
  }
}

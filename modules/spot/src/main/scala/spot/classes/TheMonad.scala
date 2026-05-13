package spot.classes
import cats.Monad
import turbolift.!!


class TheMonad[U] extends Monad[!![_, U]]:
  //// Members declared in cats.Applicative
  final override def pure[A](a: A): A !! U = !!.pure(a)

  //// Members declared in cats.FlatMap
  final override def flatMap[A, B](fa: A !! U)(f: A => B !! U): B !! U = fa.flatMap(f)

  final override def tailRecM[A, B](a: A)(f: A => Either[A, B] !! U): B !! U =
    def loop(a: A): B !! U =
      f(a).flatMap:
        case Left(a) => loop(a)
        case Right(b) => !!.pure(b)
    loop(a)

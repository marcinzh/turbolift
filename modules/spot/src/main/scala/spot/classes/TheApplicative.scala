package spot.classes
import cats.Applicative
import turbolift.!!
import spot.internals.Par


class TheApplicative[U] extends Applicative[Par[_, U]]:
  private type F[A] = Par[A, U]

  final override def pure[A](a: A): F[A] = Par.wrap(!!.pure(a))

  final override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    Par.wrap:
      val ff1 = Par.unwrap(ff)
      val fa1 = Par.unwrap(fa)
      ff1.zipWithPar(fa1)(_(_))

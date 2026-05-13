package spot
import cats.{Monad, Applicative, Parallel}
import cats.arrow.FunctionK
import turbolift.!!
import spot.classes.{TheMonad, TheApplicative}
import spot.internals.Par


object Spot:
  given theMonad[U]: Monad[!![_, U]] = new TheMonad[U]


  given theParallel[U]: Parallel[!![_, U]] = new:
    type G[A] = A !! U
    override type F[A] = Par[A, U]

    override def parallel: FunctionK[G, F] = new:
      override def apply[A](fa: G[A]): F[A] = Par.wrap(fa)

    override def sequential: FunctionK[F, G] = new:
      override def apply[A](fa: F[A]): G[A] = Par.unwrap(fa)

    override def applicative: Applicative[F] = new TheApplicative[U]

    override def monad: Monad[G] = theMonad[U]

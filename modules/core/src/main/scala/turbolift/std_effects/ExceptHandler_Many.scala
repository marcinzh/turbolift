package turbolift.std_effects
import cats.instances.either._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, Accum}
import turbolift.abstraction.typeclass.Syntax._


object ExceptHandler_Many:
  def apply[E, E1, Fx <: ExceptExt[E, E1]](fx: Fx)(implicit E: Accum[E, E1]): fx.ThisIHandler[Either[E, _]] =
    new fx.Stateless[Either[E, _]]:
      override def onReturn[A](a: A): Either[E, A] = Right(a)

      override def onTransform[M[_]: MonadPar] = new Transformed[M]:
        override def flatMap[A, B](tma: M[Either[E, A]])(f: A => M[Either[E, B]]): M[Either[E, B]] =
          tma.flatMap {
            case Right(a) => f(a)
            case Left(e) => MonadPar[M].pure(Left(e))
          }

        override def zipPar[A, B](tma: M[Either[E, A]], tmb: M[Either[E, B]]): M[Either[E, (A, B)]] =
          (tma *! tmb).map {
            case (Right(a), Right(b)) => Right((a, b))
            case (Left(e1), Left(e2)) => Left(e1 |+| e2)
            case (Left(e), _) => Left(e)
            case (_, Left(e)) => Left(e)
          }

      override def onOperation[M[_], F[_], U](implicit kk: ThisControl[M, F, U]) = new ExceptExtSig[U, E, E1]:
        override def raise[A](e: E1): A !! U =
          kk.withLift(lift => kk.pureInner(Left(E.one(e)).withRight[F[A]]))

        override def raises[A](e: E): A !! U =
          kk.withLift(lift => kk.pureInner(Left(e).withRight[F[A]]))

        override def katch[A](body: A !! U)(f: E => A !! U): A !! U =
          kk.withLift { lift =>
            lift.run(body).flatMap {
              case Right(fa) => kk.pureInner(Right(fa).withLeft[E])
              case Left(e) => lift.run(f(e))
            }
          }

    .toHandler

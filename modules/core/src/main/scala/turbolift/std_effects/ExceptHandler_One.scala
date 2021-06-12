package turbolift.std_effects
import cats.instances.either._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object ExceptHandler_One {
  def apply[E, E1, Fx <: ExceptExt[E, E1]](fx: Fx)(implicit E: E1 =:= E): fx.ThisIHandler[Either[E, *]] =
    new fx.Stateless[Either[E, *]] {
      override def onReturn[A](a: A): Either[E, A] = Right(a)

      override def onTransform[M[_]: MonadPar] = new Transformed[M] {
        override def flatMap[A, B](tma: M[Either[E, A]])(f: A => M[Either[E, B]]): M[Either[E, B]] =
          tma.flatMap {
            case Right(a) => f(a)
            case Left(e) => MonadPar[M].pure(Left(e))
          }

        override def zipPar[A, B](tma: M[Either[E, A]], tmb: M[Either[E, B]]): M[Either[E, (A, B)]] =
          (tma *! tmb).map {
            case (Right(a), Right(b)) => Right((a, b))
            case (Left(e), _) => Left(e)
            case (_, Left(e)) => Left(e)
          }
      }


      override def onOperation[M[_], F[_], U](implicit kk: ThisControl[M, F, U]) = new ExceptExtSig[U, E, E1] {
        override def raise[A](e: E1): A !! U =
          raises(E(e))

        override def raises[A](e: E): A !! U =
          kk.withLift(lift => kk.pureInner(Left(e).withRight[F[A]]))

        override def katch[A](body: A !! U)(fun: E => A !! U): A !! U =
          kk.withLift { lift =>
            lift.run(body).flatMap {
              case Right(fa) => kk.pureInner(Right(fa).withLeft[E])
              case Left(e) => lift.run(fun(e))
            }
          }
      }
    }.toHandler
}

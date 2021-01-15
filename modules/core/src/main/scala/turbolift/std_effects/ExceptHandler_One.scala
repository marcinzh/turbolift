package turbolift.std_effects
import cats.instances.either._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.Implicits.MonadParSyntax


object ExceptHandler_One {
  def apply[E, E1, Fx <: ExceptExt[E, E1]](fx: Fx)(implicit E: E1 =:= E): fx.ThisIHandler[Either[E, *]] =
    new fx.Nullary[Either[E, *]] {
      override def purer[A](a: A): Either[E, A] = Right(a)

      override def transform[M[_]: MonadPar] = new Transformed[M] {
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


      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new ExceptExtSig[U, E, E1] {
        override def raise[A](e: E1): A !! U =
          raises(E(e))

        override def raises[A](e: E): A !! U =
          ctx.withLift(lift => ctx.pureInner(Left(e).withRight[F[A]]))

        override def katch[A](scope: A !! U)(fun: E => A !! U): A !! U =
          ctx.withLift { lift =>
            lift.run(scope).flatMap {
              case Right(fa) => ctx.pureInner(Right(fa).withLeft[E])
              case Left(e) => lift.run(fun(e))
            }
          }
      }
    }.toHandler
}

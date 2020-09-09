package turbolift.std_handlers
import cats.instances.either._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, Accum}
import turbolift.abstraction.Implicits.{AccumSyntax, MonadParSyntax}
import turbolift.std_effects.{ValidationExtSig, ValidationExt}


object DefaultValidationHandler {
  def apply[E, EE, Fx <: ValidationExt[E, EE]](fx: Fx)(implicit E: Accum[E, EE]): fx.ThisIHandler[Either[EE, ?]] =
    new fx.Nullary[Either[EE, ?]] {
      override def purer[A](a: A): Either[EE, A] = Right(a)

      override def transform[M[_]: MonadPar] = new Transformed[M] {
        override def flatMap[A, B](tma: M[Either[EE, A]])(f: A => M[Either[EE, B]]): M[Either[EE, B]] =
          tma.flatMap {
            case Right(a) => f(a)
            case Left(ee) => MonadPar[M].pure(Left(ee))
          }

        override def zipPar[A, B](tma: M[Either[EE, A]], tmb: M[Either[EE, B]]): M[Either[EE, (A, B)]] =
          (tma *! tmb).map {
            case (Right(a), Right(b)) => Right((a, b))
            case (Left(ee1), Left(ee2)) => Left(ee1 |+| ee2)
            case (Left(ee), _) => Left(ee)
            case (_, Left(ee)) => Left(ee)
          }
      }

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new ValidationExtSig[U, E, EE] {
        override def invalid[A](e: E): A !! U =
          ctx.withLift(lift => ctx.pureInner(Left(E.one(e)).withRight[F[A]]))

        override def invalids[A](ee: EE): A !! U =
          ctx.withLift(lift => ctx.pureInner(Left(ee).withRight[F[A]]))

        override def validate[A](scope: A !! U)(recover: EE => A !! U): A !! U =
          ctx.withLift { lift =>
            lift.run(scope).flatMap {
              case Right(fa) => ctx.pureInner(Right(fa).withLeft[EE])
              case Left(ee) => lift.run(recover(ee))
            }
          }
      }
    }.toHandler
}

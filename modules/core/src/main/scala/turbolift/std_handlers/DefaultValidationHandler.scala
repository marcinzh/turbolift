package turbolift.std_handlers
import cats.Semigroup
import cats.syntax.semigroup._
import cats.instances.either._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, Accum}
import turbolift.abstraction.Implicits.MonadParSyntax
import turbolift.std_effects.{ValidationSig, Validation}


object DefaultValidationHandler {
  def apply[E: Semigroup, Fx <: Validation[E]](fx: Fx): fx.ThisHandler[Either[E, ?]] =
    new fx.Nullary[Either[E, ?]] {
      override def purer[A](a: A): Either[E, A] = Right(a)

      override def transform[M[_]: MonadPar] = new Transformed[M] {
        def flatMap[A, B](tma: M[Either[E, A]])(f: A => M[Either[E, B]]): M[Either[E, B]] =
          tma.flatMap {
            case Right(a) => f(a)
            case Left(e) => MonadPar[M].pure(Left(e))
          }

        def zipPar[A, B](tma: M[Either[E, A]], tmb: M[Either[E, B]]): M[Either[E, (A, B)]] =
          (tma *! tmb).map {
            case (Right(a), Right(b)) => Right((a, b))
            case (Left(e1), Left(e2)) => Left(e1 |+| e2)
            case (Left(e), _) => Left(e)
            case (_, Left(e)) => Left(e)
          }
      }

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new ValidationSig[U, E] {
        def invalid[A](e: E): A !! U =
          ctx.withLift(lift => ctx.pureInner(Left(e).withRight[F[A]]))

        def validate[A](scope: A !! U)(recover: E => A !! U): A !! U =
          ctx.withLift { lift =>
            lift.run(scope).flatMap {
              case Right(fa) => ctx.pureInner(Right(fa).withLeft[E])
              case Left(e) => lift.run(recover(e))
            }
          }
      }
    }.toHandler
}

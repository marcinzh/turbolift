package turbolift.std_handlers
import cats.instances.either._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax
import turbolift.std_effects.{ExceptSig, Except}


object DefaultExceptHandler {
  def apply[E, Fx <: Except[E]](fx: Fx) = new fx.Nullary[Either[E, ?]] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def purer[A](a: A): Either[E, A] = Right(a)

      def flatMap[A, B](tma: M[Either[E, A]])(f: A => M[Either[E, B]]): M[Either[E, B]] =
        tma.flatMap {
          case Right(a) => f(a)
          case Left(e) => MonadPar[M].pure(Left(e))
        }

      def zipPar[A, B](tma: M[Either[E, A]], tmb: M[Either[E, B]]): M[Either[E, (A, B)]] =
        (tma *! tmb).map {
          case (Right(a), Right(b)) => Right((a, b))
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
        }
    }

    def specialOps[M[_], U](context: ThisContext[M, U]) = new SpecialOps(context) with ExceptSig[U, E] {
      def raise[A](e: E): A !! U =
        withLift { l =>
          pureInner(Left(e): Either[E, Stash[A]])
        }

      def katch[A](scope: A !! U)(recover: E => A !! U): A !! U =
        withLift { l =>
          l.run(scope).flatMap {
            case Right(fa) => pureInner(Right(fa): Either[E, Stash[A]])
            case Left(e) => l.run(recover(e))
          }
        }
    }
  }.self
}

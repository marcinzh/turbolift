package turbolift.std_effects
import cats.Semigroup
import cats.syntax.semigroup._
import cats.instances.either._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.{MonadPar, Accum}
import turbolift.abstraction.implicits.MonadParSyntax


trait ValidationSig[U, E] extends Signature[U] {
  def invalid[A](e: E): A !! U
  def validate[A](scope: A !! U)(recover: E => A !! U): A !! U
}


trait Validation[E] extends Effect[ValidationSig[?, E]] {
  def invalid(e: E): Nothing !! this.type = encodeFO(_.invalid(e))
  def invalid[X](x: X)(implicit ev: Accum[X, E]): Nothing !! this.type = invalid(ev.one(x))
  def validate[A, U](scope: A !! U)(recover: E => A !! U): A !! U with this.type = encodeHO[U](_.validate(scope)(recover))

  def from[A](x: Either[E, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => invalid(e)
  }

  def handler(implicit E: Semigroup[E]) = DefaultValidationHandler[E, this.type](this)
}


object DefaultValidationHandler {
  def apply[E: Semigroup, Fx <: Validation[E]](fx: Fx) = new fx.Nullary[Either[E, ?]] {
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
          case (Left(e1), Left(e2)) => Left(e1 |+| e2)
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
        }
    }

    def specialOps[M[_], U](context: ThisContext[M, U]) = new SpecialOps(context) with ValidationSig[U, E] {
      def invalid[A](e: E): A !! U =
        withLift { l =>
          pureInner(Left(e): Either[E, Stash[A]])
        }

      def validate[A](scope: A !! U)(recover: E => A !! U): A !! U =
        withLift { l =>
          l.run(scope).flatMap {
            case Right(fa) => pureInner(Right(fa): Either[E, Stash[A]])
            case Left(e) => l.run(recover(e))
          }
        }
    }
  }.self
}

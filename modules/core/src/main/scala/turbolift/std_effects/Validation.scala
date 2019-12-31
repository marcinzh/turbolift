package turbolift.std_effects
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}


trait ValidationSig[P[_], E] extends Signature[P] {
  def invalid[A](e: E): P[A]
  def validate[A](scope: P[A])(recover: E => P[A]): P[A]
}


trait Validation[E] extends Effect[ValidationSig[?[_], E]] {
  def invalid(e: E): Nothing !! this.type = encodeFO(_.invalid(e))
  def invalid[X](x: X)(implicit ev: NonEmpty[X, E]): Nothing !! this.type = invalid(ev.nonEmpty(x))
  def validate[A, U](scope: A !! U)(recover: E => A !! U): A !! U with this.type = encodeHO[U](run => _.validate(run(scope))(e => run(recover(e))))

  def from[A](x: Either[E, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => invalid(e)
  }

  def handler(implicit E: Semigroup[E]) = ValidationHandler[E, this.type](this)
}


object ValidationHandler {
  def apply[E: Semigroup, Fx <: Validation[E]](effect: Fx) = new effect.Nullary[Either[E, ?]] {
    val theFunctor = FunctorInstances.either[E]

    def commonOps[M[_] : MonadPar] = new CommonOps[M] {
      def lift[A](ma: M[A]): M[Either[E, A]] = ma.map(Right(_))

      def flatMap[A, B](tma: M[Either[E, A]])(f: A => M[Either[E, B]]): M[Either[E, B]] =
        tma.flatMap {
          case Right(a) => f(a)
          case Left(e) => Monad[M].pure(Left(e))
        }

      def zipPar[A, B](tma: M[Either[E, A]], tmb: M[Either[E, B]]): M[Either[E, (A, B)]] =
        (tma *! tmb).map {
          case (Right(a), Right(b)) => Right((a, b))
          case (Left(e1), Left(e2)) => Left(e1 |@| e2)
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
        }
    }

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with ValidationSig[P, E] {
      def invalid[A](e: E): P[A] = liftOuter(pureInner(Left(e)))

      def validate[A](scope: P[A])(recover: E => P[A]): P[A] =
        withUnlift { run =>
          run(scope).flatMap {
            case Right(fa) => pureInner(Right(fa))
            case Left(e) => run(recover(e))
          }
        }
    }
  }.self
}

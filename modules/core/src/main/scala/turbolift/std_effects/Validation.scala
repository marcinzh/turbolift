package turbolift.std_effects
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect._


trait ValidationSig[E] extends Signature {
  def invalid(e: E): Op[Nothing]
}


trait Validation[E] extends Effect[ValidationSig[E]] with ValidationSig[E] {
  def invalid(e: E): Nothing !! this.type = encode(_.invalid(e))
  def invalid[X](x: X)(implicit ev: NonEmpty[X, E]): Nothing !! this.type = invalid(ev.nonEmpty(x))
  def validate[A, U](scope: A !! U)(recover: E => A !! U)(implicit E: Semigroup[E]) =
    handler.handle[U](scope).flatMap {
      case Right(a) => pure(a)
      case Left(e) => recover(e)
    }

  def from[A](x: Either[E, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => invalid(e)
  }

  def handler(implicit E: Semigroup[E]) = ValidationHandler[E, this.type](this)
}


object ValidationHandler {
  def apply[E: Semigroup, Fx <: Validation[E]](effect: Fx) = new effect.Nullary[Either[E, ?]] {
    def commonOps[M[_]: MonadPar] = new CommonOps[M] {
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

    def specialOps[M[_]: MonadPar] = new SpecialOps[M] with ValidationSig[E] {
      def invalid(e: E) = Monad[M].pure(Left(e))
    }
  }.self
}

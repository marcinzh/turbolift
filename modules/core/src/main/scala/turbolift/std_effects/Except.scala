package turbolift.std_effects
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect._


trait ExceptSig[E] extends Signature {
  def raise(e: E): Op[Nothing]
}

trait Except[E] extends Effect[ExceptSig[E]] with ExceptSig[E] {
  def raise(e: E) = encode(_.raise(e))
  def katch[A, U](scope: A !! U)(recover: E => A !! U) =
    handler.handle[U](scope).flatMap {
      case Right(a) => pure(a)
      case Left(e) => recover(e)
    }

  def from[A](x: Either[E, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => raise(e)
  }

  val handler = ExceptHandler[E, this.type](this)
}


object ExceptHandler {
  def apply[E, Fx <: Except[E]](effect: Fx) = new effect.Nullary[Either[E, ?]] {
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
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
        }
    }

    def specialOps[M[_]: MonadPar] = new SpecialOps[M] with ExceptSig[E] {
      def raise(e: E) = Monad[M].pure(Left(e))
    }
  }.self
}
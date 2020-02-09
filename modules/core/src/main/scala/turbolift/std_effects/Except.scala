package turbolift.std_effects
import cats.instances.either._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax


trait ExceptSig[U, E] extends Signature[U] {
  def raise[A](e: E): A !! U
  def katch[A](scope: A !! U)(recover: E => A !! U): A !! U
}


trait Except[E] extends Effect[ExceptSig[?, E]] {
  def raise(e: E): Nothing !! this.type = encodeFO(_.raise(e))
  def katch[A, U](scope: A !! U)(recover: E => A !! U): A !! U with this.type = encodeHO[U](_.katch(scope)(recover))

  def from[A](x: Either[E, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => raise(e)
  }

  val handler = DefaultExceptHandler[E, this.type](this)
}


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

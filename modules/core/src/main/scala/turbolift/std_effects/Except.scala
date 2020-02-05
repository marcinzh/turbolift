package turbolift.std_effects
import cats.instances.either._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax


trait ExceptSig[P[_], E] extends Signature[P] {
  def raise[A](e: E): P[A]
  def katch[A](scope: P[A])(recover: E => P[A]): P[A]
}


trait Except[E] extends Effect[ExceptSig[?[_], E]] {
  def raise(e: E): Nothing !! this.type = encodeFO(_.raise(e))
  
  def katch[A, U](scope: A !! U)(recover: E => A !! U): A !! U with this.type =
    encodeHO[U](run => _.katch(run(scope))(e => run(recover(e))))

  def from[A](x: Either[E, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => raise(e)
  }

  val handler = DefaultExceptHandler[E, this.type](this)
}


object DefaultExceptHandler {
  def apply[E, Fx <: Except[E]](fx: Fx) = new fx.Nullary[Either[E, ?]] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      // def pure[A](a: A): M[Either[E, A]] = M.pure(Right(a))
      
      def purer[A](a: A): Either[E, A] = Right(a)

      def lift[A](ma: M[A]): M[Either[E, A]] = ma.map(Right(_))

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

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with ExceptSig[P, E] {
      def raise[A](e: E): P[A] = liftOuter(pureInner(Left(e)))

      def katch[A](scope: P[A])(recover: E => P[A]): P[A] =
        withUnlift { run =>
          run(scope).flatMap {
            case Right(fa) => pureInner(Right(fa))
            case Left(e) => run(recover(e))
          }
        }
    }
  }.self
}

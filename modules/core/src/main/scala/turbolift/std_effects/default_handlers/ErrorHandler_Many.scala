package turbolift.std_effects.default_handlers
import turbolift.!!
import turbolift.typeclass.{MonadZip, Accum}
import turbolift.typeclass.Syntax._
import turbolift.std_effects.{ErrorEffect, ErrorSig}


private[std_effects] object ErrorHandler_Many:
  def apply[E, E1, Fx <: ErrorEffect[E, E1]](fx: Fx)(implicit E: Accum[E, E1]): fx.ThisHandler.Free[Either[E, _]] =
    new fx.Stateless[Either[E, _]] with ErrorSig[E, E1]:
      override def onPure[A](a: A): Either[E, A] = Right(a)

      override def onFlatMap[A, B, M[_]: MonadZip](tma: M[Either[E, A]])(f: A => M[Either[E, B]]): M[Either[E, B]] =
        tma.flatMap {
          case Right(a) => f(a)
          case Left(e) => MonadZip[M].pure(Left(e))
        }

      override def onZip[A, B, M[_]: MonadZip](tma: M[Either[E, A]], tmb: M[Either[E, B]]): M[Either[E, (A, B)]] =
        (tma *! tmb).map {
          case (Right(a), Right(b)) => Right((a, b))
          case (Left(e1), Left(e2)) => Left(e1 |+| e2)
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
        }

      override def raise(e: E1): Nothing !@! ThisEffect =
        kk ?=> kk.outer(Left(E.one(e)))

      override def raises(e: E): Nothing !@! ThisEffect =
        kk ?=> kk.outer(Left(e))

      override def katch[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !@! U =
        kk ?=> kk.locally(body).flatMap {
          case Right(fa) => kk.outer(Right(fa).withLeft[E])
          case Left(e) => kk.locally(f(e))
        }

    .toHandler

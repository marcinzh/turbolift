package turbolift.effects.default_handlers
import turbolift.!!
import turbolift.typeclass.Accum
import turbolift.typeclass.Syntax._
import turbolift.effects.{ErrorEffect, ErrorSignature}


extension [E, E1](fx: ErrorEffect[E, E1])
  private[effects] def errorHandler_all(using E: Accum[E, E1]): fx.ThisHandler.Free[Either[E, _]] =
    new fx.Stateless[Either[E, _]] with fx.Parallel with ErrorSignature[E, E1]:
      override def onPure[A](a: A): Either[E, A] = Right(a)

      override def onUnpure[A](aa: Either[E, A]): A !! ThisEffect =
        aa match
          case Right(a) => !!.pure(a)
          case Left(e) => fx.raises(e)

      override def onZip[A, B, C](ea: Either[E, A], eb: Either[E, B], k: (A, B) => C): Either[E, C] =
        (ea, eb) match
          case (Right(a), Right(b)) => Right(k(a, b))
          case (Left(e1), Left(e2)) => Left(e1 |+| e2)
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)

      override def raise(e: E1): Nothing !@! ThisEffect = raises(E.one(e))

      override def raises(e: E): Nothing !@! ThisEffect = _ => !!.pure(Left(e))

      override def catchAll[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !@! U =
        k => k.local(body).flatMap {
          case (Right(a), k) => k(a)
          case (Left(e), k) => k.escape(f(e)).flatMap {
            case (a, k) => k(a)
          }
        }

    .toHandler

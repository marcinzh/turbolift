package turbolift.effects.default_handlers
import turbolift.!!
import turbolift.typeclass.One
import turbolift.effects.{ErrorEffect, ErrorSignature}


extension [E, E1](fx: ErrorEffect[E, E1])
  private[effects] def errorHandler_first(using E: One[E, E1]): fx.ThisHandler.Free[Either[E, _]] =
    new fx.impl.Free.Stateless[Either[E, _]] with fx.impl.Sequential with ErrorSignature[E, E1]:
      override def onReturn[A](a: A): Either[E, A] !! Any = !!.pure(Right(a))

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

package turbolift.handlers
import turbolift.!!
import turbolift.typeclass.One
import turbolift.effects.{ErrorEffect, ErrorSignature}


extension [E, E1](fx: ErrorEffect[E, E1])
  def errorHandler_first(using E: One[E, E1]): fx.ThisHandler.FromId.Free[Either[E, _]] =
    new fx.impl.Stateless.FromId.Free[Either[E, _]] with fx.impl.Sequential.Restartable with ErrorSignature[E, E1]:
      override def onReturn(a: Unknown): Either[E, Unknown] !! Any = !!.pure(Right(a))

      override def onRestart(aa: Either[E, Unknown]): Unknown !! ThisEffect =
        aa match
          case Right(a) => !!.pure(a)
          case Left(e) => fx.raises(e)

      override def raise(e: E1): Nothing !@! ThisEffect = raises(E.one(e))

      override def raises(e: E): Nothing !@! ThisEffect = k => k.abort(Left(e))

      override def toEither[A, U <: ThisEffect](body: A !! U): Either[E, A] !@! U =
        k => k.localAndResume(body)

      override def catchAllEff[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !@! U =
        k => k.local(body).flatMap:
          case (Right(a), k) => k(a)
          case (Left(e), k) => k.escapeAndResume(f(e))
          // case (Left(e), k) => k.local(f(e)).flatMap:
          //   case (Right(a), k) => k(a)
          //   case (Left(e), k) => !!.pure(Left(e))

    .toHandler

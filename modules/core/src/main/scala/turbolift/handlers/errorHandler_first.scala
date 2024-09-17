package turbolift.handlers
import turbolift.!!
import turbolift.typeclass.One
import turbolift.effects.{ErrorEffect, ErrorSignature}
import turbolift.Extensions._


extension [E, E1](fx: ErrorEffect[E, E1])
  def errorHandler_first(using E: One[E, E1]): fx.ThisHandler[Identity, Either[E, _], Any] =
    new fx.impl.Stateless[Identity, Either[E, _], Any] with fx.impl.Sequential.Restartable with ErrorSignature[E, E1]:
      override def onReturn(a: Unknown): Either[E, Unknown] !! Any = !!.pure(Right(a))

      override def onRestart(aa: Either[E, Unknown]): Unknown !! fx.type =
        aa match
          case Right(a) => !!.pure(a)
          case Left(e) => fx.raises(e)

      override def raise(e: E1): Nothing !! ThisEffect = raises(E.one(e))

      override def raises(e: E): Nothing !! ThisEffect = Control.abort(Left(e))

      override def toEither[A, U <: ThisEffect](body: A !! U): Either[E, A] !! U = Control.delimit(body)

      override def catchAllEff[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !! U =
        Control.delimit(body).flatMap:
          case Right(a) => !!.pure(a)
          case Left(e) => f(e)

    .toHandler

package turbolift.handlers
import turbolift.!!
import turbolift.typeclass.One
import turbolift.effects.{ErrorEffect, ErrorSignature}
import turbolift.Extensions._


extension [E, E1](fx: ErrorEffect[E, E1])
  def errorHandler_first(using E: One[E, E1]): fx.ThisHandler[Identity, Either[E, _], Any] =
    new fx.impl.Stateless[Identity, Either[E, _], Any] with fx.impl.Sequential.Restartable with ErrorSignature[E, E1]:
      override def onReturn(a: Unknown): Either[E, Unknown] !! Any = !!.pure(Right(a))

      override def onRestart(aa: Either[E, Unknown]): Unknown !! ThisEffect =
        aa match
          case Right(a) => !!.pure(a)
          case Left(e) => fx.raises(e)

      override def raise(e: E1): Nothing !@! ThisEffect = raises(E.one(e))

      override def raises(e: E): Nothing !@! ThisEffect = k => k.abort(Left(e))

      override def toEither[A, U <: ThisEffect](body: A !! U): Either[E, A] !@! U =
        k => k.delimitAndResume(body)

      override def catchAllEff[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !@! U =
        k => k.delimit(body).flatMap:
          case (Right(a), k) => k(a)
          case (Left(e), k) => k.escapeAndResume(f(e))
          // case (Left(e), k) => k.delimit(f(e)).flatMap:
          //   case (Right(a), k) => k(a)
          //   case (Left(e), k) => !!.pure(Left(e))

    .toHandler

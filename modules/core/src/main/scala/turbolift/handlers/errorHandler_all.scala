package turbolift.handlers
import turbolift.!!
import turbolift.typeclass.Accum
import turbolift.typeclass.Syntax._
import turbolift.effects.{ErrorEffect, ErrorSignature}
import turbolift.Extensions._


extension [E, E1](fx: ErrorEffect[E, E1])
  def errorHandler_all(using E: Accum[E, E1]): fx.ThisHandler[Identity, Either[E, _], Any] =
    new fx.impl.Stateless[Identity, Either[E, _], Any] with fx.impl.Parallel with ErrorSignature[E, E1]:
      override def onReturn(a: Unknown): Either[E, Unknown] !! Any = !!.pure(Right(a))

      override def onRestart(aa: Either[E, Unknown]): Unknown !! ThisEffect =
        aa match
          case Right(a) => !!.pure(a)
          case Left(e) => fx.raises(e)

      override def onZip[A, B, C](ea: Either[E, A], eb: Either[E, B], k: (A, B) => C): Either[E, C] =
        (ea, eb) match
          case (Right(a), Right(b)) => Right(k(a, b))
          case (Left(e1), Left(e2)) => Left(e1 |+| e2)
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)

      override def raise(e: E1): Nothing !! ThisEffect = raises(E.one(e))

      override def raises(e: E): Nothing !! ThisEffect = Control.abort(Left(e))

      override def toEither[A, U <: ThisEffect](body: A !! U): Either[E, A] !! U = Control.delimit(body)

      override def catchAllEff[A, U <: ThisEffect](body: A !! U)(f: E => A !! U): A !! U =
        Control.delimit(body).flatMap:
          case Right(a) => !!.pure(a)
          case Left(e) => f(e)

    .toHandler

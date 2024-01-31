package turbolift.handlers
import turbolift.!!
import turbolift.typeclass.Accum
import turbolift.typeclass.Syntax._
import turbolift.effects.{ErrorEffect, ErrorSignature}


extension [E, E1](fx: ErrorEffect[E, E1])
  def errorHandler_all(using E: Accum[E, E1]): fx.ThisHandler.FromId.Free[Either[E, _]] =
    new fx.impl.Stateless.FromId.Free[Either[E, _]] with fx.impl.Parallel with ErrorSignature[E, E1]:
      override def onReturn(a: Unknown): Either[E, Unknown] !! Any = !!.pure(Right(a))

      override def onReintro(aa: Either[E, Unknown]): Unknown !! ThisEffect =
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

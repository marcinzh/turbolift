package turbolift.std_effects
import cats.instances.option._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.Implicits.MonadParSyntax


object FailHandler {
  def apply[Fx <: Fail](fx: Fx): fx.ThisIHandler[Option] =
    new fx.Nullary[Option] {
      override def purer[A](a: A): Option[A] = Some(a)

      override def transform[M[_]: MonadPar] = new Transformed[M] {
        override def flatMap[A, B](tma: M[Option[A]])(f: A => M[Option[B]]): M[Option[B]] =
          tma.flatMap {
            case Some(a) => f(a)
            case None => MonadPar[M].pure(None)
          }

        override def zipPar[A, B](tma: M[Option[A]], tmb: M[Option[B]]): M[Option[(A, B)]] =
          (tma *! tmb).map {
            case (Some(a), Some(b)) => Some((a, b))
            case _ => None
          }
      }

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new FailSig[U] {
        override def empty[A]: A !! U =
          ctx.withLift(lift => ctx.pureInner(None: Option[F[A]]))

        override def plus[A](lhs: A !! U, rhs: => A !! U): A !! U =
          ctx.withLift { lift =>
            lift.run(lhs).flatMap { x =>
              if (x.isDefined)
                ctx.pureInner(x)
              else
                lift.run(rhs)
            }
          }

        override def each[A](as: Iterable[A]): A !! U =
          if (as.isEmpty)
            empty
          else
            plus(!!.pure(as.head), each(as.tail))
      }
    }.toHandler
}

package turbolift.std_effects
import cats.instances.option._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object ChoiceHandler_One:
  def apply[Fx <: ChoiceExt](fx: Fx): fx.ThisIHandler[Option] =
    new fx.Stateless[Option]:
      override def onReturn[A](a: A): Option[A] = Some(a)

      override def onTransform[M[_]: MonadPar] = new Transformed[M]:
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

      override def onOperation[M[_], F[_], U](implicit kk: ThisControl[M, F, U]) = new ChoiceSig[U]:
        override def empty[A]: A !! U =
          kk.withLift(lift => kk.pureInner(None: Option[F[A]]))

        override def plus[A](lhs: A !! U, rhs: => A !! U): A !! U =
          kk.withLift { lift =>
            lift.run(lhs).flatMap { x =>
              if x.isDefined
              then kk.pureInner(x)
              else lift.run(rhs)
            }
          }

        override def each[A](as: Iterable[A]): A !! U =
          if as.isEmpty
          then empty
          else plus(!!.pure(as.head), each(as.tail))
      
    .toHandler

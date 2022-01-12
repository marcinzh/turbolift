package turbolift.std_effects
import cats.instances.option._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object ChoiceHandler_One:
  def apply[Fx <: ChoiceExt](fx: Fx): fx.ThisIHandler[Option] =
    new fx.Stateless[Option] with ChoiceSig:
      override def onReturn[A](a: A): Option[A] = Some(a)

      override def onFlatMap[A, B, M[_]: MonadPar](tma: M[Option[A]])(f: A => M[Option[B]]): M[Option[B]] =
        tma.flatMap {
          case Some(a) => f(a)
          case None => MonadPar[M].pure(None)
        }

      override def onProduct[A, B, M[_]: MonadPar](tma: M[Option[A]], tmb: M[Option[B]]): M[Option[(A, B)]] =
        (tma *! tmb).map {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }

      override def empty: Nothing !@! ThisEffect =
        kk ?=> kk.outer(None)

      override def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !@! U =
        kk ?=> kk.locally(lhs).flatMap { x =>
          if x.isDefined
          then kk.outer(x)
          else kk.locally(rhs)
        }

      override def each[A](as: Iterable[A]): A !@! ThisEffect =
        //@#@
        kk ?=> kk.outer(as.headOption.map(kk.inner(_)))
        // if as.isEmpty
        // then empty
        // else plus(!!.pure(as.head), each(as.tail))
      
    .toHandler

package turbolift.std_effects.default_handlers
import turbolift.!!
import turbolift.typeclass.MonadZip
import turbolift.typeclass.Syntax._
import turbolift.std_effects.{ChoiceEffect, ChoiceSig}


private[std_effects] object ChoiceHandler_One:
  def apply[Fx <: ChoiceEffect](fx: Fx): fx.ThisHandler.Free[Option] =
    new fx.Stateless[Option] with ChoiceSig:
      override def onPure[A](a: A): Option[A] = Some(a)

      override def onFlatMap[A, B, M[_]: MonadZip](tma: M[Option[A]])(f: A => M[Option[B]]): M[Option[B]] =
        tma.flatMap {
          case Some(a) => f(a)
          case None => MonadZip[M].pure(None)
        }

      override def onZip[A, B, M[_]: MonadZip](tma: M[Option[A]], tmb: M[Option[B]]): M[Option[(A, B)]] =
        (tma *! tmb).map {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }

      override def fail: Nothing !@! ThisEffect =
        kk ?=> kk.outer(None)

      override def orElse[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !@! U =
        kk ?=> kk.locally(lhs).flatMap { xs =>
          if xs.isDefined
          then kk.outer(xs)
          else kk.locally(rhs)
        }

      override def choose[A](as: Iterable[A]): A !@! ThisEffect =
        // orElse(!!.pure(as.head), choose(as.tail))
        kk ?=> 
          if as.isEmpty
          then kk.outer(None)
          else
            kk.locally(!!.pure(as.head)).flatMap { xs =>
              if xs.isDefined
              then kk.outer(xs)
              else kk.locally(fx.choose(as.tail))
            }
      
    .toHandler

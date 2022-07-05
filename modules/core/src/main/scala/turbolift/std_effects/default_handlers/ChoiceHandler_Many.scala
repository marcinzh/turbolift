package turbolift.std_effects.default_handlers
import turbolift.!!
import turbolift.typeclass.MonadZip
import turbolift.typeclass.Syntax._
import turbolift.std_effects.{ChoiceEffect, ChoiceSig}


private[std_effects] object ChoiceHandler_Many:
  def apply[Fx <: ChoiceEffect](fx: Fx): fx.ThisHandler.Free[Vector] =
    new fx.Stateless[Vector] with ChoiceSig:
      override def onPure[A](a: A): Vector[A] = Vector(a)

      override def onFlatMap[A, B, M[_]: MonadZip](tma: M[Vector[A]])(f: A => M[Vector[B]]): M[Vector[B]] =
        def loop(as: Vector[A]): M[Vector[B]] =
          as match
            case Vector() => MonadZip[M].pure(Vector())
            case Vector(a) => f(a)
            case _ =>
              val (as1, as2) = as.splitAt(as.size / 2)
              (loop(as1) *! loop(as2)).map {
                case (bs1, bs2) => bs1 ++ bs2
              }
        tma.flatMap(loop)

      override def onZip[A, B, M[_]: MonadZip](tma: M[Vector[A]], tmb: M[Vector[B]]): M[Vector[(A, B)]] =
        (tma *! tmb).map {
          case (as, bs) =>
            for
              a <- as
              b <- bs
            yield (a, b)
        }

      override def fail: Nothing !@! ThisEffect =
        kk ?=> kk.outer(Vector.empty)

      override def choose[A](as: Iterable[A]): A !@! ThisEffect =
        kk ?=> kk.outer(as.iterator.map(kk.inner).toVector)

      override def orElse[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !@! U =
        kk ?=> (kk.locally(lhs) *! kk.locally(rhs)).map {
          case (xs, ys) => xs ++ ys
        }

    .toHandler

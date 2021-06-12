package turbolift.std_effects
import cats.instances.vector._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object ChoiceHandler_Many {
  def apply[Fx <: ChoiceExt](fx: Fx): fx.ThisIHandler[Vector] =
    new fx.Stateless[Vector] {
      override def onReturn[A](a: A): Vector[A] = Vector(a)

      override def onTransform[M[_]: MonadPar] = new Transformed[M] {
        override def flatMap[A, B](tma: M[Vector[A]])(f: A => M[Vector[B]]): M[Vector[B]] = {
          def loop(as: Vector[A]): M[Vector[B]] = as match {
            case Vector() => MonadPar[M].pure(Vector())
            case Vector(a) => f(a)
            case _ =>
              val (as1, as2) = as.splitAt(as.size / 2)
              (loop(as1) *! loop(as2)).map {
                case (bs1, bs2) => bs1 ++ bs2
              }
          }
          tma.flatMap(loop)
        }

        override def zipPar[A, B](tma: M[Vector[A]], tmb: M[Vector[B]]): M[Vector[(A, B)]] =
          (tma *! tmb).map {
            case (as, bs) =>
              for {
                a <- as
                b <- bs
              } yield (a, b)
          }
      }

      override def onOperation[M[_], F[_], U](implicit kk: ThisControl[M, F, U]) = new ChoiceSig[U] {
        override def empty[A]: A !! U =
          kk.withLift(lift => kk.pureInner(Vector.empty[F[A]]))

        override def each[A](as: Iterable[A]): A !! U =
          kk.withLift(lift => kk.pureInner(as.iterator.map(lift.pureStash).toVector))

        override def plus[A](lhs: A !! U, rhs: => A !! U): A !! U =
          kk.withLift { lift =>
            (lift.run(lhs) *! lift.run(rhs)).map {
              case (xs, ys) => xs ++ ys
            }
          }

      }
    }.toHandler
}

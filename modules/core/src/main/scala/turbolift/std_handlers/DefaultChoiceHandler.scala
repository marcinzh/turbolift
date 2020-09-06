package turbolift.std_handlers
import cats.instances.vector._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.Implicits.MonadParSyntax
import turbolift.std_effects.{ChoiceSig, Choice}


object DefaultChoiceHandler {
  def apply[Fx <: Choice](fx: Fx): fx.ThisHandler[Vector] =
    new fx.Nullary[Vector] {
      override def purer[A](a: A): Vector[A] = Vector(a)

      override def transform[M[_]: MonadPar] = new Transformed[M] {
        def flatMap[A, B](tma: M[Vector[A]])(f: A => M[Vector[B]]): M[Vector[B]] = {
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

        def zipPar[A, B](tma: M[Vector[A]], tmb: M[Vector[B]]): M[Vector[(A, B)]] =
          (tma *! tmb).map {
            case (as, bs) =>
              for {
                a <- as
                b <- bs
              } yield (a, b)
          }
      }

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new ChoiceSig[U] {
        def empty[A]: A !! U =
          ctx.withLift(lift => ctx.pureInner(Vector.empty[F[A]]))

        def each[A](as: Iterable[A]): A !! U =
          ctx.withLift(lift => ctx.pureInner(as.iterator.map(lift.pureStash).toVector))

        def plus[A](lhs: A !! U, rhs: => A !! U): A !! U =
          ctx.withLift { lift =>
            (lift.run(lhs) *! lift.run(rhs)).map {
              case (xs, ys) => xs ++ ys
            }
          }

      }
    }.toHandler
}

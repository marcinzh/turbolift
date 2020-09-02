package turbolift.std_handlers
import cats.instances.vector._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.Implicits.MonadParSyntax
import turbolift.std_effects.{ChoiceSig, Choice}


object DefaultChoiceHandler {
  def apply[Fx <: Choice](fx: Fx): fx.ThisHandler[Vector] =
    new fx.Nullary[Vector] {
      def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
        def purer[A](a: A): Vector[A] = Vector(a)

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

      def specialOps[M[_], U](context: ThisContext[M, U]) = new SpecialOps(context) with ChoiceSig[U] {
        def empty[A]: A !! U =
          withLift { l =>
            pureInner(Vector.empty[Stash[A]])
          }

        def plus[A](lhs: A !! U, rhs: => A !! U): A !! U =
          withLift { l =>
            (l.run(lhs) *! l.run(rhs)).map {
              case (xs, ys) => xs ++ ys
            }
          }

        def each[A](as: Iterable[A]): A !! U =
          withLift { l =>
            pureInner(as.iterator.map(l.pureStash).toVector)
          }
      }
    }.toHandler
}

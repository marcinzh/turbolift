package turbolift.std_effects
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.typeclass.Syntax._


object ChoiceHandler_Many:
  def apply[Fx <: ChoiceExt](fx: Fx): fx.ThisIHandler[Vector] =
    new fx.Stateless[Vector] with ChoiceSig:
      override def onReturn[A](a: A): Vector[A] = Vector(a)

      override def onFlatMap[A, B, M[_]: MonadPar](tma: M[Vector[A]])(f: A => M[Vector[B]]): M[Vector[B]] =
        def loop(as: Vector[A]): M[Vector[B]] =
          as match
            case Vector() => MonadPar[M].pure(Vector())
            case Vector(a) => f(a)
            case _ =>
              val (as1, as2) = as.splitAt(as.size / 2)
              (loop(as1) *! loop(as2)).map {
                case (bs1, bs2) => bs1 ++ bs2
              }
        tma.flatMap(loop)

      override def onProduct[A, B, M[_]: MonadPar](tma: M[Vector[A]], tmb: M[Vector[B]]): M[Vector[(A, B)]] =
        (tma *! tmb).map {
          case (as, bs) =>
            for
              a <- as
              b <- bs
            yield (a, b)
        }

      override def empty: Nothing !@! ThisEffect =
        kk ?=> kk.outer(Vector.empty)

      override def each[A](as: Iterable[A]): A !@! ThisEffect =
        kk ?=> kk.outer(as.iterator.map(kk.inner).toVector)

      override def plus[A, U <: ThisEffect](lhs: A !! U, rhs: => A !! U): A !@! U =
        kk ?=> (kk.locally(lhs) *! kk.locally(rhs)).map {
          case (xs, ys) => xs ++ ys
        }

    .toHandler

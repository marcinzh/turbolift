package turbolift.std_effects
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{FilterableEffect, FailSig}


trait NonDetSig[P[_]] extends FailSig[P] {
  def each[A](as: Iterable[A]): P[A]
}


trait NonDet extends FilterableEffect[NonDetSig] {
  def each[A](as: Iterable[A]) = encodeFO(_.each(as))
  
  def from[A](as: A*) = each(as.toVector)

  val handler = DefaultNonDetHandler(this)
}


object DefaultNonDetHandler {
  def apply[Fx <: NonDet](fx: Fx) = new fx.Nullary[Vector] {
    val theFunctor = FunctorInstances.vector

    def commonOps[M[_]: MonadPar] = new CommonOps[M] {
      def lift[A](ma: M[A]): M[Vector[A]] = ma.map(Vector(_))

      def flatMap[A, B](tma: M[Vector[A]])(f: A => M[Vector[B]]): M[Vector[B]] = {
        def loop(as: Vector[A]): M[Vector[B]] = as match {
          case Vector() => Monad[M].pure(Vector())
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

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with NonDetSig[P] {
      def fail[A]: P[A] = liftOuter(pureInner(Vector()))

      def each[A](as: Iterable[A]): P[A] = liftOuter(pureInner(as.toVector))

      def orElseSeq[A](lhs: P[A], rhs: => P[A]): P[A] =
        withUnlift { run =>
          run(lhs).flatMap { x =>
            if (x.nonEmpty)
              pureInner(x)
            else
              run(rhs)
          }
        }

      def orElsePar[A](lhs: P[A], rhs: P[A]): P[A] =
        withUnlift { run =>
          (run(lhs) *! run(rhs)).map {
            case (xs, ys) => if (xs.nonEmpty) xs else ys
          }
        }
    }
  }.self
}

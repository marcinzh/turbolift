package turbolift.std_effects
import cats.instances.vector._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, AlternativeSig}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax


trait ChoiceSig[P[_]] extends AlternativeSig[P] {
  def each[A](as: Iterable[A]): P[A]
}


trait Choice extends Effect.Alternative[ChoiceSig] {
  def each[A](as: Iterable[A]) = encodeFO(_.each(as))
  
  def fromEach[A](as: A*) = each(as.toVector)

  val handler = DefaultChoiceHandler(this)
}


object DefaultChoiceHandler {
  def apply[Fx <: Choice](fx: Fx) = new fx.Nullary[Vector] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def purer[A](a: A): Vector[A] = Vector(a)

      def lift[A](ma: M[A]): M[Vector[A]] = ma.map(Vector(_))

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

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with ChoiceSig[P] {
      def empty[A]: P[A] =
        withLift { l =>
          pureInner(Vector())
        }

      def plus[A](lhs: P[A], rhs: => P[A]): P[A] =
        withLift { l =>
          (l.run(lhs) *! l.run(rhs)).map {
            case (xs, ys) => xs ++ ys
          }
        }

      def each[A](as: Iterable[A]): P[A] =
        withLift { l =>
          pureInner(as.iterator.map(l.pureStash).toVector)
        }
    }
  }.self
}

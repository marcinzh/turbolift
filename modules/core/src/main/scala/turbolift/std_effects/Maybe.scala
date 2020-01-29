package turbolift.std_effects
import mwords._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, AlternativeSig}


trait MaybeSig[P[_]] extends AlternativeSig[P]


trait Maybe extends Effect.Alternative[MaybeSig] {
  val fail: Nothing !! this.type = empty
  
  val handler = DefaultMaybeHandler(this)
}


object DefaultMaybeHandler {
  def apply[Fx <: Maybe](fx: Fx) = new fx.Nullary[Option] {
    val theFunctor = FunctorInstances.option

    def commonOps[M[_]: MonadPar] = new CommonOps[M] {
      def lift[A](ma: M[A]): M[Option[A]] = ma.map(Some(_))

      def flatMap[A, B](tma: M[Option[A]])(f: A => M[Option[B]]): M[Option[B]] =
        tma.flatMap {
          case Some(a) => f(a)
          case None => Monad[M].pure(None)
        }

      def zipPar[A, B](tma: M[Option[A]], tmb: M[Option[B]]): M[Option[(A, B)]] =
        (tma *! tmb).map {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }
    }

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with MaybeSig[P] {
      def empty[A]: P[A] = liftOuter(pureInner(None))

      def plus[A](lhs: P[A], rhs: => P[A]): P[A] =
        withUnlift { run =>
          run(lhs).flatMap { x =>
            if (x.isDefined)
              pureInner(x)
            else
              run(rhs)
          }
        }
    }
  }.self
}

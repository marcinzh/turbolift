package turbolift.std_effects
// import cats.implicits._
import cats.instances.option._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, AlternativeSig}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax


trait FailSig[P[_]] extends AlternativeSig[P]


trait Fail extends Effect.Alternative[FailSig] {
  val fail: Nothing !! this.type = empty
  
  val handler = DefaultFailHandler(this)
}


object DefaultFailHandler {
  def apply[Fx <: Fail](fx: Fx) = new fx.Nullary[Option] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      // def pure[A](a: A): M[Option[A]] = M.pure(Some(a))

      def purer[A](a: A): Option[A] = Some(a)

      def lift[A](ma: M[A]): M[Option[A]] = ma.map(Some(_))

      def flatMap[A, B](tma: M[Option[A]])(f: A => M[Option[B]]): M[Option[B]] =
        tma.flatMap {
          case Some(a) => f(a)
          case None => MonadPar[M].pure(None)
        }

      def zipPar[A, B](tma: M[Option[A]], tmb: M[Option[B]]): M[Option[(A, B)]] =
        (tma *! tmb).map {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }
    }

    def specialOps[M[_], P[_]](context: ThisContext[M, P]) = new SpecialOps(context) with FailSig[P] {
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

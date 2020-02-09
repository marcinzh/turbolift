package turbolift.std_effects
// import cats.implicits._
import cats.instances.option._
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, AlternativeSig}
import turbolift.abstraction.typeclass.MonadPar
import turbolift.abstraction.implicits.MonadParSyntax


trait FailSig[U] extends AlternativeSig[U]


trait Fail extends Effect.Alternative[FailSig] {
  val fail: Nothing !! this.type = empty
  
  val handler = DefaultFailHandler(this)
}


object DefaultFailHandler {
  def apply[Fx <: Fail](fx: Fx) = new fx.Nullary[Option] {
    def commonOps[M[_]](implicit M: MonadPar[M]) = new CommonOps[M] {
      def purer[A](a: A): Option[A] = Some(a)

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

    def specialOps[M[_], U](context: ThisContext[M, U]) = new SpecialOps(context) with FailSig[U] {
      def empty[A]: A !! U =
        withLift { l =>
          pureInner(None: Option[Stash[A]])
        }

      def plus[A](lhs: A !! U, rhs: => A !! U): A !! U =
        withLift { l =>
          l.run(lhs).flatMap { x =>
            if (x.isDefined)
              pureInner(x)
            else
              l.run(rhs)
          }
        }
    }
  }.self
}

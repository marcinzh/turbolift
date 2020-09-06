package turbolift.std_handlers
import cats.Monoid
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, AccumZero}
import turbolift.abstraction.Implicits.{AccumSyntax, MonadParSyntax}
import turbolift.std_effects.{AccumulatorSig, Accumulator}


object DefaultAccumulatorHandler {
  def apply[E, W, Fx <: Accumulator[E]](fx: Fx)(implicit W: AccumZero[E, W]): fx.ThisIHandler[(W, ?)] =
    new fx.Unary[W, (W, ?)] {
      override def purer[A](w: W, a: A): (W, A) = (w, a)

      override def transform[M[_]: MonadPar] = new Transformed[M] {
        override def flatMap[A, B](tma: W => M[(W, A)])(f: A => W => M[(W, B)]): W => M[(W, B)] =
          w0 => tma(w0).flatMap {
            case (w1, a) => f(a)(w1)
          }

        override def zipPar[A, B](tma: W => M[(W, A)], tmb: W => M[(W, B)]): W => M[(W, (A, B))] =
          w0 => (tma(W.zero) *! tmb(W.zero)).map {
            case ((w1, a), (w2, b)) => ((w0 |+| w1) |+| w2, (a, b))
          }
      }

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new AccumulatorSig[U, E] {
        override def tell(e: E): Unit !! U =
          ctx.withLift(lift => w0 => ctx.pureInner((w0 |+ e, lift.unitStash())))

        override def clear[A](scope: A !! U): A !! U =
          ctx.withLift { lift => w0 =>
            lift.run(scope)(W.zero).map {
              case (_, fa) => (w0, fa)
            }
          }
      }
    }.toHandler(W.zero)
}

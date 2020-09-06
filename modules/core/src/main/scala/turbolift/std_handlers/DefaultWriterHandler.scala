package turbolift.std_handlers
import cats.Monoid
import cats.syntax.monoid._
import cats.syntax.functor._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, Accum}
import turbolift.abstraction.Implicits.MonadParSyntax
import turbolift.std_effects.{WriterSig, Writer}


object DefaultWriterHandler {
  def apply[W, Fx <: Writer[W]](fx: Fx)(implicit W: Monoid[W]): fx.ThisHandler[(W, ?)] =
    new fx.Unary[W, (W, ?)] {
      override def purer[A](w: W, a: A): (W, A) = (w, a)
  
      override def transform[M[_]: MonadPar] = new Transformed[M] {
        override def flatMap[A, B](tma: W => M[(W, A)])(f: A => W => M[(W, B)]): W => M[(W, B)] =
          w0 => tma(w0).flatMap {
            case (w1, a) => f(a)(w1)
          }

        override def zipPar[A, B](tma: W => M[(W, A)], tmb: W => M[(W, B)]): W => M[(W, (A, B))] =
          w0 => (tma(W.empty) *! tmb(W.empty)).map {
            case ((w1, a), (w2, b)) => ((w0 |+| w1) |+| w2, (a, b))
          }
      }

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new WriterSig[U, W] {
        override def tell(w: W): Unit !! U =
          ctx.withLift(lift => w0 => ctx.pureInner((w0 |+| w, lift.unitStash())))

        override def listen[A](scope: A !! U): (W, A) !! U =
          ctx.withLift { lift => w0 =>
            lift.run(scope)(W.empty).map {
              case (w, fa) => (w0 |+| w, fa.map((w, _)))
            }
          }

        override def censor[A](scope: A !! U)(mod: W => W): A !! U =
          ctx.withLift { lift => w0 =>
            lift.run(scope)(W.empty).map {
              case (w, fa) => (w0 |+| mod(w), fa)
            }
          }

        override def clear[A](scope: A !! U): A !! U =
          ctx.withLift { lift => w0 =>
            lift.run(scope)(W.empty).map {
              case (_, fa) => (w0, fa)
            }
          }
      }
    }.toHandler(W.empty)
}

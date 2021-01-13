package turbolift.std_handlers
import cats.syntax.functor._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, AccumZero}
import turbolift.abstraction.Implicits.{AccumSyntax, MonadParSyntax}
import turbolift.std_effects.{WriterExtSig, WriterExt}


object DefaultWriterHandler {
  def apply[W, W1, Fx <: WriterExt[W, W1]](fx: Fx)(implicit W: AccumZero[W, W1]): fx.ThisIHandler[(W, ?)] =
    new fx.Unary[W, (W, ?)] {
      override def purer[A](w: W, a: A): (W, A) = (w, a)
  
      override def transform[M[_]: MonadPar] = new Transformed[M] {
        override def flatMap[A, B](tma: W => M[(W, A)])(f: A => W => M[(W, B)]): W => M[(W, B)] =
          w0 => tma(w0).flatMap {
            case (w, a) => f(a)(w)
          }

        override def zipPar[A, B](tma: W => M[(W, A)], tmb: W => M[(W, B)]): W => M[(W, (A, B))] =
          w0 => (tma(W.zero) *! tmb(W.zero)).map {
            case ((w, a), (w2, b)) => ((w0 |+| w) |+| w2, (a, b))
          }
      }

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new WriterExtSig[U, W, W1] {
        override def tell(w: W1): Unit !! U =
          ctx.withLift(lift => w0 => ctx.pureInner((w0 |+ w, lift.unitStash())))

        override def tells(w: W): Unit !! U =
          ctx.withLift(lift => w0 => ctx.pureInner((w0 |+| w, lift.unitStash())))

        override def listen[A](scope: A !! U): (W, A) !! U =
          ctx.withLift { lift => w0 =>
            lift.run(scope)(W.zero).map {
              case (w, fa) => (w0 |+| w, fa.map((w, _)))
            }
          }

        override def censor[A](scope: A !! U)(mod: W => W): A !! U =
          ctx.withLift { lift => w0 =>
            lift.run(scope)(W.zero).map {
              case (w, fa) => (w0 |+| mod(w), fa)
            }
          }

        override def mute[A](scope: A !! U): A !! U =
          ctx.withLift { lift => w0 =>
            lift.run(scope)(W.zero).map {
              case (_, fa) => (w0, fa)
            }
          }
      }
    }.toHandler(W.zero)
}

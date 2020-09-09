package turbolift.std_handlers
import cats.syntax.functor._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, AccumZero}
import turbolift.abstraction.Implicits.{AccumSyntax, MonadParSyntax}
import turbolift.std_effects.{WriterExtSig, WriterExt}


object DefaultWriterHandler {
  def apply[W, WW, Fx <: WriterExt[W, WW]](fx: Fx)(implicit W: AccumZero[W, WW]): fx.ThisIHandler[(WW, ?)] =
    new fx.Unary[WW, (WW, ?)] {
      override def purer[A](ww: WW, a: A): (WW, A) = (ww, a)
  
      override def transform[M[_]: MonadPar] = new Transformed[M] {
        override def flatMap[A, B](tma: WW => M[(WW, A)])(f: A => WW => M[(WW, B)]): WW => M[(WW, B)] =
          ww0 => tma(ww0).flatMap {
            case (ww1, a) => f(a)(ww1)
          }

        override def zipPar[A, B](tma: WW => M[(WW, A)], tmb: WW => M[(WW, B)]): WW => M[(WW, (A, B))] =
          ww0 => (tma(W.zero) *! tmb(W.zero)).map {
            case ((ww1, a), (ww2, b)) => ((ww0 |+| ww1) |+| ww2, (a, b))
          }
      }

      override def interpret[M[_], F[_], U](implicit ctx: ThisContext[M, F, U]) = new WriterExtSig[U, W, WW] {
        override def tell(w: W): Unit !! U =
          ctx.withLift(lift => ww0 => ctx.pureInner((ww0 |+ w, lift.unitStash())))

        override def tells(ww: WW): Unit !! U =
          ctx.withLift(lift => ww0 => ctx.pureInner((ww0 |+| ww, lift.unitStash())))

        override def listen[A](scope: A !! U): (WW, A) !! U =
          ctx.withLift { lift => ww0 =>
            lift.run(scope)(W.zero).map {
              case (ww, fa) => (ww0 |+| ww, fa.map((ww, _)))
            }
          }

        override def censor[A](scope: A !! U)(mod: WW => WW): A !! U =
          ctx.withLift { lift => ww0 =>
            lift.run(scope)(W.zero).map {
              case (ww, fa) => (ww0 |+| mod(ww), fa)
            }
          }

        override def mute[A](scope: A !! U): A !! U =
          ctx.withLift { lift => ww0 =>
            lift.run(scope)(W.zero).map {
              case (_, fa) => (ww0, fa)
            }
          }
      }
    }.toHandler(W.zero)
}

package turbolift.std_effects
import cats.syntax.functor._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, AccumZero}
import turbolift.abstraction.typeclass.Syntax._


object WriterHandler:
  def apply[W, W1, Fx <: WriterExt[W, W1]](fx: Fx)(implicit W: AccumZero[W, W1]): fx.ThisIHandler[(W, _)] =
    new fx.Stateful[W, (W, _)]:
      override def onReturn[A](w: W, a: A): (W, A) = (w, a)
  
      override def onTransform[M[_]: MonadPar] = new Transformed[M]:
        override def flatMap[A, B](tma: W => M[(W, A)])(f: A => W => M[(W, B)]): W => M[(W, B)] =
          w0 => tma(w0).flatMap {
            case (w, a) => f(a)(w)
          }

        override def zipPar[A, B](tma: W => M[(W, A)], tmb: W => M[(W, B)]): W => M[(W, (A, B))] =
          w0 => (tma(W.zero) *! tmb(W.zero)).map {
            case ((w, a), (w2, b)) => ((w0 |+| w) |+| w2, (a, b))
          }
      
      override def onOperation[M[_], F[_], U](implicit kk: ThisControl[M, F, U]) = new WriterExtSig[U, W, W1]:
        override def tell(w: W1): Unit !! U =
          kk.withLift(lift => w0 => kk.pureInner((w0 |+ w, lift.unitStash())))

        override def tells(w: W): Unit !! U =
          kk.withLift(lift => w0 => kk.pureInner((w0 |+| w, lift.unitStash())))

        override def listen[A](body: A !! U): (W, A) !! U =
          kk.withLift { lift => w0 =>
            lift.run(body)(W.zero).map {
              case (w, fa) => (w0 |+| w, fa.map((w, _)))
            }
          }

        override def censor[A](body: A !! U)(mod: W => W): A !! U =
          kk.withLift { lift => w0 =>
            lift.run(body)(W.zero).map {
              case (w, fa) => (w0 |+| mod(w), fa)
            }
          }

        override def mute[A](body: A !! U): A !! U =
          kk.withLift { lift => w0 =>
            lift.run(body)(W.zero).map {
              case (_, fa) => (w0, fa)
            }
          }

    .toHandler(W.zero)

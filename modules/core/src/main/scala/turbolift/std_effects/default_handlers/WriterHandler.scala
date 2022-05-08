package turbolift.std_effects.default_handlers
import cats.syntax.functor._
import turbolift.!!
import turbolift.typeclass.{MonadPar, AccumZero}
import turbolift.typeclass.Syntax._
import turbolift.std_effects.{WriterExt, WriterExtSig}
import FlippedPairFunctor.given


private[std_effects] object WriterHandler:
  def apply[W, W1, Fx <: WriterExt[W, W1]](fx: Fx)(implicit W: AccumZero[W, W1]): fx.ThisHandler.Free[(_, W)] =
    new fx.Stateful[W, (_, W)] with WriterExtSig[W, W1]:
      override def onReturn[A](a: A) = (a, _)

      override def onFlatMap[A, B, M[_]: MonadPar](tma: W => M[(A, W)])(f: A => W => M[(B, W)]): W => M[(B, W)] =
        w0 => tma(w0).flatMap {
          case (a, w) => f(a)(w)
        }

      override def onProduct[A, B, M[_]: MonadPar](tma: W => M[(A, W)], tmb: W => M[(B, W)]): W => M[((A, B), W)] =
        w0 => (tma(w0) *! tmb(W.zero)).map {
          case ((a, w1), (b, w2)) => ((a, b), w1 |+| w2)
        }
  
      override def tell(w: W1): Unit !@! ThisEffect =
        kk ?=> w0 => kk.outer((kk.inner(), w0 |+ w))

      override def tells(w: W): Unit !@! ThisEffect =
        kk ?=> w0 => kk.outer((kk.inner(), w0 |+| w))

      override def mute[A, U <: ThisEffect](body: A !! U): A !@! U =
        kk ?=> w0 => kk.locally(body)(W.zero).map {
          case (aa, _) => (aa, w0)
        }

      override def listen[A, U <: ThisEffect](body: A !! U): (A, W) !@! U =
        kk ?=> w0 => kk.locally(body)(W.zero).map {
          case (aa, w) => (aa.map((_, w)), w0 |+| w)
        }

      override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !@! U =
        kk ?=> w0 => kk.locally(body)(W.zero).map {
          case (aa, w) => (aa, w0 |+| f(w))
        }

    .toHandler(W.zero)

package turbolift.std_effects
import cats.syntax.functor._
import cats.instances.tuple._
import turbolift.abstraction.!!
import turbolift.abstraction.typeclass.{MonadPar, AccumZero}
import turbolift.abstraction.typeclass.Syntax._


object WriterHandler:
  def apply[W, W1, Fx <: WriterExt[W, W1]](fx: Fx)(implicit W: AccumZero[W, W1]): fx.ThisIHandler[(W, _)] =
    new fx.Stateful[W, (W, _)] with WriterExtSig[W, W1]:
      override def onReturn[A](a: A) = (_, a)

      override def onFlatMap[A, B, M[_]: MonadPar](tma: W => M[(W, A)])(f: A => W => M[(W, B)]): W => M[(W, B)] =
        w0 => tma(w0).flatMap {
          case (w, a) => f(a)(w)
        }

      override def onProduct[A, B, M[_]: MonadPar](tma: W => M[(W, A)], tmb: W => M[(W, B)]): W => M[(W, (A, B))] =
        w0 => (tma(w0) *! tmb(W.zero)).map {
          case ((w1, a), (w2, b)) => (w1 |+| w2, (a, b))
        }
  
      override def tell(w: W1): Unit !@! ThisEffect =
        kk ?=> w0 => kk.outer((w0 |+ w, kk.inner()))

      override def tells(w: W): Unit !@! ThisEffect =
        kk ?=> w0 => kk.outer((w0 |+| w, kk.inner()))

      override def mute[A, U <: ThisEffect](body: A !! U): A !@! U =
        kk ?=> w0 => kk.locally(body)(W.zero).map {
          case (_, aa) => (w0, aa)
        }

      override def listen[A, U <: ThisEffect](body: A !! U): (W, A) !@! U =
        kk ?=> w0 => kk.locally(body)(W.zero).map {
          case (w, aa) => (w0 |+| w, aa.map((w, _)))
        }

      override def censor[A, U <: ThisEffect](body: A !! U)(f: W => W): A !@! U =
        kk ?=> w0 => kk.locally(body)(W.zero).map {
          case (w, aa) => (w0 |+| f(w), aa)
        }

    .toHandler(W.zero)

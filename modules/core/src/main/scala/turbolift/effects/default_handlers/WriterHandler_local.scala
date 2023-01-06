package turbolift.effects.default_handlers
import turbolift.!!
import turbolift.typeclass.AccumZero
import turbolift.typeclass.Syntax._
import turbolift.effects.{WriterEffect, WriterSig}


extension [W, W1](fx: WriterEffect[W, W1])
  private[effects] def writerHandler_local(implicit W: AccumZero[W, W1]): fx.ThisHandler.Free[(_, W)] =
    new fx.Stateful[W, (_, W)] with fx.Parallel.ForkJoin with WriterSig[W, W1]:
      override def onPure[A](a: A, w: W): (A, W) = (a, w)

      override def onUnpure[A](a_w: (A, W)): A !! ThisEffect = fx.tells(a_w._2) &&! !!.pure(a_w._1)

      override def onZip[A, B, C](a_w: (A, W), b_w: (B, W), k: (A, B) => C): (C, W) = (k(a_w._1, b_w._1), a_w._2 |+| b_w._2)

      override def onFork(w: W): (W, W) = (w, W.zero)
      
      override def tell(w: W1): Unit !@! ThisEffect = (k, w0) => k((), w0 |+ w)

      override def tells(w: W): Unit !@! ThisEffect = (k, w0) => k((), w0 |+| w)

      override def mute[A, U <: ThisEffect](body: A !! U): A !@! U = censor(_ => W.zero)(body)

      override def listen[A, U <: ThisEffect](body: A !! U): (A, W) !@! U =
        (k, _) => k.local(body, W.zero).flatMap {
          case (a_w @ (_, w), k) => k(a_w, k.get |+| w)
        }

      override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !@! U =
        (k, _) => k.local(body, W.zero).flatMap {
          case ((a, w), k) => k(a, k.get |+| f(w))
        }

      override def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !@! U =
        (k, _) => k.local(body, W.zero).flatMap {
          case (((a, f), w), k) => k(a, k.get |+| f(w))
        }

    .toHandler(W.zero)

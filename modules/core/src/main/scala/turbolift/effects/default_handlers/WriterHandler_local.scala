package turbolift.effects.default_handlers
import turbolift.!!
import turbolift.typeclass.AccumZero
import turbolift.typeclass.Syntax._
import turbolift.effects.{WriterEffect, WriterSignature}


extension [W, W1](fx: WriterEffect[W, W1])
  private[effects] def writerHandler_local(implicit W: AccumZero[W, W1]): fx.ThisHandler.Free[(_, W)] =
    new fx.Free.Stateful[W, (_, W)] with fx.Parallel.ForkJoin with WriterSignature[W, W1]:
      override def onReturn[A](a: A, w: W): (A, W) !! Any = !!.pure((a, w))

      override def onUnpure[A](a_w: (A, W)): A !! ThisEffect = fx.tells(a_w._2) &&! !!.pure(a_w._1)

      override def onZip[A, B, C](a_w: (A, W), b_w: (B, W), k: (A, B) => C): (C, W) = (k(a_w._1, b_w._1), a_w._2 |+| b_w._2)

      override def onFork(w: W): (W, W) = (w, W.zero)
      
      override def tell(w: W1): Unit !@! ThisEffect = (k, w0) => k((), w0 |+ w)

      override def tells(w: W): Unit !@! ThisEffect = (k, w0) => k((), w0 |+| w)

      override def mute[A, U <: ThisEffect](body: A !! U): A !@! U = censor(_ => W.zero)(body)

      override def listen[A, U <: ThisEffect](body: A !! U): (A, W) !@! U =
        (k, _) => k.local(body, W.zero).flatMap {
          case (a_w2 @ (_, w2), k, w1) => k(a_w2, w1 |+| w2)
        }

      override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !@! U =
        (k, _) => k.local(body, W.zero).flatMap {
          case ((a, w2), k, w1) => k(a, w1 |+| f(w2))
        }

      override def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !@! U =
        (k, _) => k.local(body, W.zero).flatMap {
          case (((a, f), w2), k, w1) => k(a, w1 |+| f(w2))
        }

    .toHandler(W.zero)

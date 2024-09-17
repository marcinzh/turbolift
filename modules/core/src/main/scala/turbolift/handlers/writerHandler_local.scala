package turbolift.handlers
import turbolift.!!
import turbolift.typeclass.AccumZero
import turbolift.typeclass.Syntax._
import turbolift.effects.{WriterEffect, WriterSignature}
import turbolift.Extensions._


extension [W, W1](fx: WriterEffect[W, W1])
  def writerHandler_local(implicit W: AccumZero[W, W1]): fx.ThisHandler[Identity, (_, W), Any] =
    new fx.impl.Stateful[Identity, (_, W), Any] with fx.impl.Parallel.ForkJoin with WriterSignature[W, W1]:
      override type Local = W
      
      override def onInitial: W !! Any = !!.pure(W.zero)

      override def onReturn(a: Unknown, w: W): (Unknown, W) !! Any = !!.pure((a, w))

      override def onRestart(a_w: (Unknown, W)): Unknown !! fx.type = fx.tells(a_w._2) &&! !!.pure(a_w._1)

      override def onZip[A, B, C](a_w: (A, W), b_w: (B, W), k: (A, B) => C): (C, W) = (k(a_w._1, b_w._1), a_w._2 |+| b_w._2)

      override def onFork(w: W): (W, W) = (w, W.zero)
      
      override def tell(w: W1): Unit !! ThisEffect = Local.modify(_ |+ w)

      override def tells(w: W): Unit !! ThisEffect = Local.modify(_ |+| w)

      override def mute[A, U <: ThisEffect](body: A !! U): A !! U = censor(_ => W.zero)(body)

      override def listen[A, U <: ThisEffect](body: A !! U): (A, W) !! U =
        Control.delimitPut(body, W.zero).flatMap:
          case aw @ (a, w) => Local.modify(_ |+| w).as(aw)

      override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !! U =
        Control.delimitPut(body, W.zero).flatMap:
          case aw @ (a, w) => Local.modify(_ |+| f(w)).as(a)

      override def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !! U =
        Control.delimitPut(body, W.zero).flatMap:
          case ((a, f), w) => Local.modify(_ |+| f(w)).as(a)

    .toHandler

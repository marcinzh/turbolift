package turbolift.handlers
import turbolift.!!
import turbolift.typeclass.AccumZero
import turbolift.typeclass.Syntax._
import turbolift.effects.{WriterEffect, WriterSignature, IO}
import turbolift.io.AtomicVar
import turbolift.Extensions._


extension [W, W1](fx: WriterEffect[W, W1])
  def writerHandler_shared(implicit W: AccumZero[W, W1]): fx.ThisHandler[Identity, (_, W), IO] =
    AtomicVar(W.zero) >>=! { avar =>
      new fx.impl.Proxy[IO] with WriterSignature[W, W1]:
        override def tell(w: W1): Unit !! ThisEffect = avar.modify(_ |+ w)

        override def tells(w: W): Unit !! ThisEffect = avar.modify(_ |+| w) 

        override def mute[A, U <: ThisEffect](body: A !! U): A !! U = censor(_ => W.zero)(body)

        override def listen[A, U <: ThisEffect](body: A !! U): (A, W) !! U =
          for
            w0 <- avar.swap(W.zero)
            a <- body
            w1 <- avar.getModify(w0 |+| _)
          yield (a, w1)

        override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !! U =
          for
            w0 <- avar.swap(W.zero)
            a <- body
            _ <- avar.modify(w => w0 |+| f(w))
          yield a

        override def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !! U =
          for
            w0 <- avar.swap(W.zero)
            workaround <- body
            (a, f) = workaround
            _ <- avar.modify(w => w0 |+| f(w))
          yield a

      .toHandler
      .mapEffK([A] => (a: A) => avar.get.map((a, _)))
    }

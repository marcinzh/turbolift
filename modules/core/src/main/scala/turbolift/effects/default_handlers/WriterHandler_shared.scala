package turbolift.effects.default_handlers
import turbolift.!!
import turbolift.io.{IO, Ref}
import turbolift.typeclass.AccumZero
import turbolift.typeclass.Syntax._
import turbolift.effects.{WriterEffect, WriterSignature}


extension [W, W1](fx: WriterEffect[W, W1])
  private[effects] def writerHandler_shared(implicit W: AccumZero[W, W1]): fx.ThisHandler[(_, W), IO] =
    Ref(W.zero) >>=! { ref =>
      new fx.Proxy[IO] with WriterSignature[W, W1]:
        override def tell(w: W1): Unit !@! ThisEffect = _ => ref.modify(_ |+ w)

        override def tells(w: W): Unit !@! ThisEffect = _ => ref.modify(_ |+| w) 

        override def mute[A, U <: ThisEffect](body: A !! U): A !@! U = censor(_ => W.zero)(body)

        override def listen[A, U <: ThisEffect](body: A !! U): (A, W) !@! U =
          kk =>
            for
              w0 <- ref.swap(W.zero)
              a <- kk.escape(body)
              w1 <- ref.getModify(w0 |+| _)
            yield (a, w1)

        override def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !@! U =
          kk =>
            for
              w0 <- ref.swap(W.zero)
              a <- kk.escape(body)
              _ <- ref.modify(w => w0 |+| f(w))
            yield a

        override def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !@! U =
          kk =>
            for
              w0 <- ref.swap(W.zero)
              workaround <- kk.escape(body)
              (a, f) = workaround
              _ <- ref.modify(w => w0 |+| f(w))
            yield a

      .toHandler
      .flatMap([A] => (a: A) => ref.get.map((a, _)))
    }

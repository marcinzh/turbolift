package turbolift.effects
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.typeclass.AccumZero
import turbolift.typeclass.Syntax._
import turbolift.io.AtomicVar


trait WriterSignature[W, W1] extends Signature:
  def tell(w: W1): Unit !! ThisEffect
  def tells(w: W): Unit !! ThisEffect
  def mute[A, U <: ThisEffect](body: A !! U): A !! U
  def listen[A, U <: ThisEffect](body: A !! U): (A, W) !! U
  def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !! U
  def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !! U


trait WriterEffect[W, W1] extends Effect[WriterSignature[W, W1]] with WriterSignature[W, W1]:
  enclosing =>
  final override def tell(w: W1): Unit !! this.type = perform(_.tell(w))
  final override def tells(w: W): Unit !! this.type = perform(_.tells(w))
  final override def mute[A, U <: this.type](body: A !! U): A !! U = perform(_.mute(body))
  final override def listen[A, U <: this.type](body: A !! U): (A, W) !! U = perform(_.listen(body))
  final override def censor[A, U <: this.type](f: W => W)(body: A !! U): A !! U = perform(_.censor(f)(body))
  final override def pass[A, U <: this.type](body: (A, W => W) !! U): A !! U = perform(_.pass(body))

  final def tell[K, V1](k: K, v: V1)(using ev: ((K, V1)) <:< W1): Unit !! this.type = tell(ev((k, v)))

  /** Predefined handlers for this effect. */
  object handlers:
    def local(using W: AccumZero[W, W1]): Handler[Identity, (_, W), enclosing.type, Any] =
      new impl.Stateful[Identity, (_, W), Any] with impl.Parallel.ForkJoin with WriterSignature[W, W1]:
        override type Local = W
        override def onInitial: W !! Any = !!.pure(W.zero)
        override def onReturn(a: Unknown, w: W): (Unknown, W) !! Any = !!.pure((a, w))
        override def onRestart(a_w: (Unknown, W)): Unknown !! enclosing.type = enclosing.tells(a_w._2) &&! !!.pure(a_w._1)
        override def onUnknown(aa: (Unknown, W)): Option[Unknown] = Some(aa._1)

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


    def shared(using W: AccumZero[W, W1]): Handler[Identity, (_, W), enclosing.type, IO] =
      AtomicVar(W.zero).flatMapHandler: avar =>
        new impl.Proxy[IO] with WriterSignature[W, W1]:
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


    /** Lile [[local]], but accumulate with given function instead of typeclass. */
    def localFold(using W =:= W1)(zero: W, plus: (W, W1) => W): Handler[Identity, (_, W), enclosing.type, Any] = local(using AccumZero.instanceEq(zero, plus))

    /** Lile [[shared]], but accumulate with given function instead of typeclass. */
    def sharedFold(using W =:= W1)(zero: W, plus: (W, W1) => W): Handler[Identity, (_, W), enclosing.type, IO] = shared(using AccumZero.instanceEq(zero, plus))


trait Writer[W] extends WriterEffect[W, W]:
  export handlers.{local => handler}

trait WriterK[F[_], W] extends WriterEffect[F[W], W]:
  export handlers.{local => handler}

trait WriterG[M[_, _], K, V] extends WriterEffect[M[K, V], (K, V)]:
  export handlers.{local => handler}

trait WriterGK[M[_, _], K, F[_], V] extends WriterEffect[M[K, F[V]], (K, V)]:
  export handlers.{local => handler}

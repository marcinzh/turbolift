package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.Extensions._
import turbolift.typeclass.AccumZero
import turbolift.handlers.{writerHandler_local, writerHandler_shared}


trait WriterSignature[W, W1] extends Signature:
  def tell(w: W1): Unit !! ThisEffect
  def tells(w: W): Unit !! ThisEffect
  def mute[A, U <: ThisEffect](body: A !! U): A !! U
  def listen[A, U <: ThisEffect](body: A !! U): (A, W) !! U
  def censor[A, U <: ThisEffect](f: W => W)(body: A !! U): A !! U
  def pass[A, U <: ThisEffect](body: (A, W => W) !! U): A !! U


trait WriterEffect[W, W1] extends Effect[WriterSignature[W, W1]] with WriterSignature[W, W1]:
  final override def tell(w: W1): Unit !! this.type = perform(_.tell(w))
  final override def tells(w: W): Unit !! this.type = perform(_.tells(w))
  final override def mute[A, U <: this.type](body: A !! U): A !! U = perform(_.mute(body))
  final override def listen[A, U <: this.type](body: A !! U): (A, W) !! U = perform(_.listen(body))
  final override def censor[A, U <: this.type](f: W => W)(body: A !! U): A !! U = perform(_.censor(f)(body))
  final override def pass[A, U <: this.type](body: (A, W => W) !! U): A !! U = perform(_.pass(body))

  final def tell[K, V1](k: K, v: V1)(using ev: ((K, V1)) <:< W1): Unit !! this.type = tell(ev((k, v)))

  /** Predefined handlers for this effect. */
  object handlers:
    def local(using W: AccumZero[W, W1]): ThisHandler[Identity, (_, W), Any] = WriterEffect.this.writerHandler_local
    def shared(using W: AccumZero[W, W1]): ThisHandler[Identity, (_, W), IO] = WriterEffect.this.writerHandler_shared
    def localFold(using W =:= W1)(zero: W, plus: (W, W1) => W): ThisHandler[Identity, (_, W), Any] = local(using AccumZero.instanceEq(zero, plus))
    def sharedFold(using W =:= W1)(zero: W, plus: (W, W1) => W): ThisHandler[Identity, (_, W), IO] = shared(using AccumZero.instanceEq(zero, plus))


trait Writer[W] extends WriterEffect[W, W]:
  export handlers.{local => handler}

trait WriterK[F[_], W] extends WriterEffect[F[W], W]:
  export handlers.{local => handler}

trait WriterG[M[_, _], K, V] extends WriterEffect[M[K, V], (K, V)]:
  export handlers.{local => handler}

trait WriterGK[M[_, _], K, F[_], V] extends WriterEffect[M[K, F[V]], (K, V)]:
  export handlers.{local => handler}

package turbolift.std_effects
import turbolift.abstraction.{!!, Effect, Signature}
import turbolift.abstraction.typeclass.AccumZero


trait WriterExtSig[W, W1] extends Signature:
  def tell(w: W1): Unit !@! ThisEffect
  def tells(w: W): Unit !@! ThisEffect
  def mute[A, U <: ThisEffect](body: A !! U): A !@! U
  def listen[A, U <: ThisEffect](body: A !! U): (W, A) !@! U
  def censor[A, U <: ThisEffect](body: A !! U)(f: W => W): A !@! U


trait WriterExt[W, W1] extends Effect[WriterExtSig[W, W1]] with WriterExtSig[W, W1]:
  final override def tell(w: W1): Unit !! this.type = impure(_.tell(w))
  final override def tells(w: W): Unit !! this.type = impure(_.tells(w))
  final override def mute[A, U <: this.type](body: A !! U): A !! U = impure(_.mute(body))
  final override def listen[A, U <: this.type](body: A !! U): (W, A) !! U = impure(_.listen(body))
  final override def censor[A, U <: this.type](body: A !! U)(f: W => W): A !! U = impure(_.censor(body)(f))

  final def tell[K, V1](k: K, v: V1)(implicit ev: ((K, V1)) <:< W1): Unit !! this.type = tell(ev((k, v)))

  def handler(implicit W: AccumZero[W, W1]): ThisIHandler[(W, _)] = WriterHandler[W, W1, this.type](this)

trait Writer[W] extends WriterExt[W, W]

trait WriterK[F[_], W] extends WriterExt[F[W], W]

trait WriterG[M[_, _], K, V] extends WriterExt[M[K, V], (K, V)]

trait WriterGK[M[_, _], K, F[_], V] extends WriterExt[M[K, F[V]], (K, V)]

type WriterSig[W] = WriterExtSig[W, W]

type WriterKSig[F[_], W] = WriterExtSig[F[W], W]

type WriterGSig[M[_, _], K, V] = WriterExtSig[M[K, V], (K, V)]

type WriterGKSig[M[_, _], K, F[_], V] = WriterExtSig[M[K, F[V]], (K, V)]

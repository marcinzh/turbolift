package turbolift.std_effects
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.AccumZero


trait WriterExtSig[U, W, W1] {
  def tell(w: W1): Unit !! U
  def tells(w: W): Unit !! U
  def mute[A](scope: A !! U): A !! U
  def listen[A](scope: A !! U): (W, A) !! U
  def censor[A](scope: A !! U)(mod: W => W): A !! U
}

trait WriterExt[W, W1] extends Effect[WriterExtSig[?, W, W1]] {
  final def tell(w1: W1): Unit !! this.type = embedFO(_.tell(w1))
  final def tells(w: W): Unit !! this.type = embedFO(_.tells(w))
  final def listen[A, U](scope: A !! U): (W, A) !! U with this.type = embedHO[U](_.listen(scope))
  final def censor[A, U](scope: A !! U)(f: W => W): A !! U with this.type = embedHO[U](_.censor(scope)(f))
  final def mute[A, U](scope: A !! U): A !! U with this.type = embedHO[U](_.mute(scope))

  def handler(implicit W: AccumZero[W, W1]): ThisIHandler[(W, ?)] = WriterHandler[W, W1, this.type](this)
}

trait Writer[W] extends WriterExt[W, W]

trait WriterK[F[_], W] extends WriterExt[F[W], W]

trait WriterExports {
  type WriterSig[U, W] = WriterExtSig[U, W, W]

  type WriterKSig[U, F[_], W] = WriterExtSig[U, F[W], W]
}

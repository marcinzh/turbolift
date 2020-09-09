package turbolift.std_effects
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.AccumZero
import turbolift.std_handlers.DefaultWriterHandler


trait WriterExtSig[U, W, WW] {
  def tell(w: W): Unit !! U
  def tells(ww: WW): Unit !! U
  def mute[A](scope: A !! U): A !! U
  def listen[A](scope: A !! U): (WW, A) !! U
  def censor[A](scope: A !! U)(mod: WW => WW): A !! U
}

trait WriterExt[W, WW] extends Effect[WriterExtSig[?, W, WW]] {
  final def tell(w: W): Unit !! this.type = embedFO(_.tell(w))
  final def tells(ww: WW): Unit !! this.type = embedFO(_.tells(ww))
  final def listen[A, U](scope: A !! U): (WW, A) !! U with this.type = embedHO[U](_.listen(scope))
  final def censor[A, U](scope: A !! U)(f: WW => WW): A !! U with this.type = embedHO[U](_.censor(scope)(f))
  final def mute[A, U](scope: A !! U): A !! U with this.type = embedHO[U](_.mute(scope))

  def handler(implicit W: AccumZero[W, WW]): ThisIHandler[(WW, ?)] = DefaultWriterHandler[W, WW, this.type](this)
}

trait Writer[W] extends WriterExt[W, W]

trait WriterK[F[_], W] extends WriterExt[W, F[W]]

trait WriterExports {
  type WriterSig[U, W] = WriterExtSig[U, W, W]

  type WriterKSig[U, F[_], W] = WriterExtSig[U, W, F[W]]
}

package turbolift.std_effects
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.internals.effect.Signature
import turbolift.std_handlers.DefaultReaderHandler


trait ReaderSig[U, R] extends Signature[U] {
  def ask: R !! U
  def local[A](mod: R => R)(scope: A !! U): A !! U
}

trait Reader[R] extends Effect[ReaderSig[?, R]] {
  final val ask: R !! this.type = embedFO(_.ask)
  final def asks[A](f: R => A): A !! this.type = ask.map(f)
  final def local[A, U](mod: R => R)(scope: A !! U): A !! U with this.type = embedHO[U](_.local(mod)(scope))

  val handler = DefaultReaderHandler[R, this.type](this)
}

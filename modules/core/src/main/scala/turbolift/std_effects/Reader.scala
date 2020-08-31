package turbolift.std_effects
import turbolift.abstraction.!!
import turbolift.abstraction.effect.{Effect, Signature}
import turbolift.std_handlers.DefaultReaderHandler


trait ReaderSig[U, R] extends Signature[U] {
  def ask: R !! U
  def local[A](mod: R => R)(scope: A !! U): A !! U
}

trait Reader[R] extends Effect[ReaderSig[?, R]] {
  val ask: R !! this.type = encodeFO(_.ask)
  def asks[A](f: R => A): A !! this.type = ask.map(f)
  def local[A, U](mod: R => R)(scope: A !! U): A !! U with this.type = encodeHO[U](_.local(mod)(scope))

  val handler = DefaultReaderHandler[R, this.type](this)
}

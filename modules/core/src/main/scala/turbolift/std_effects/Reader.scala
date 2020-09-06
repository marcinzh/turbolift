package turbolift.std_effects
import cats.Id
import turbolift.abstraction.{!!, Effect}
import turbolift.std_handlers.DefaultReaderHandler


trait ReaderSig[U, R] {
  def ask: R !! U
  def asks[A](f: R => A): A !! U
  def local[A](r: R)(scope: A !! U): A !! U
  def localModify[A](mod: R => R)(scope: A !! U): A !! U
}

trait Reader[R] extends Effect[ReaderSig[?, R]] {
  final val ask: R !! this.type = embedFO(_.ask)
  final def asks[A](f: R => A): A !! this.type = embedFO(_.asks(f))
  final def local[A, U](r: R)(scope: A !! U): A !! U with this.type = embedHO[U](_.local(r)(scope))
  final def localModify[A, U](mod: R => R)(scope: A !! U): A !! U with this.type = embedHO[U](_.localModify(mod)(scope))

  def handler(initial: R): ThisHandler[Id] = DefaultReaderHandler(this, initial)
}

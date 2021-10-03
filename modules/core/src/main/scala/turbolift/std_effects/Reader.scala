package turbolift.std_effects
import cats.Id
import turbolift.abstraction.{!!, Effect}


trait ReaderSig[U, R]:
  def ask: R !! U
  def asks[A](f: R => A): A !! U
  def localPut[A](r: R)(body: A !! U): A !! U
  def localModify[A](f: R => R)(body: A !! U): A !! U


trait Reader[R] extends Effect[ReaderSig[_, R]]:
  final val ask: R !! this.type = impureFO(_.ask)
  final def asks[A](f: R => A): A !! this.type = impureFO(_.asks(f))
  final def localPut[A, U <: this.type](r: R)(body: A !! U): A !! U = impureHO[U](_.localPut(r)(body))
  final def localModify[A, U <: this.type](f: R => R)(body: A !! U): A !! U = impureHO[U](_.localModify(f)(body))

  def handler(initial: R): ThisIHandler[Id] = ReaderHandler(this, initial)

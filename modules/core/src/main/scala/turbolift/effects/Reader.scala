package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.effects.default_handlers.readerHandler


trait ReaderSig[R] extends Signature:
  def ask: R !@! ThisEffect
  def asks[A](f: R => A): A !@! ThisEffect
  def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !@! U
  def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !@! U


trait Reader[R] extends Effect[ReaderSig[R]] with ReaderSig[R]:
  final override val ask: R !! this.type = perform(_.ask)
  final override def asks[A](f: R => A): A !! this.type = perform(_.asks(f))
  final override def localPut[A, U <: this.type](r: R)(body: A !! U): A !! U = perform(_.localPut(r)(body))
  final override def localModify[A, U <: this.type](f: R => R)(body: A !! U): A !! U = perform(_.localModify(f)(body))

  /** Default handler for this effect. */
  def handler(initial: R): ThisHandler.FreeId = this.readerHandler(initial)

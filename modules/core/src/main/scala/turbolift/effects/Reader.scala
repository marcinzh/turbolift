package turbolift.effects
import turbolift.{!!, Effect, Signature}
import turbolift.Extensions._
import turbolift.handlers.readerHandler


trait ReaderSignature[R] extends Signature:
  def ask: R !! ThisEffect
  def asks[A](f: R => A): A !! ThisEffect
  def asksEff[A, U <: ThisEffect](f: R => A !! U): A !! U
  def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !! U
  def localPutEff[A, U <: ThisEffect](r: R !! U)(body: A !! U): A !! U
  def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !! U
  def localModifyEff[A, U <: ThisEffect](f: R => R !! U)(body: A !! U): A !! U


trait ReaderEffect[R] extends Effect[ReaderSignature[R]] with ReaderSignature[R]:
  final override val ask: R !! this.type = perform(_.ask)
  final override def asks[A](f: R => A): A !! this.type = perform(_.asks(f))
  final override def asksEff[A, U <: this.type](f: R => A !! U): A !! U = perform(_.asksEff(f))
  final override def localPut[A, U <: this.type](r: R)(body: A !! U): A !! U = perform(_.localPut(r)(body))
  final override def localPutEff[A, U <: this.type](r: R !! U)(body: A !! U): A !! U = perform(_.localPutEff(r)(body))
  final override def localModify[A, U <: this.type](f: R => R)(body: A !! U): A !! U = perform(_.localModify(f)(body))
  final override def localModifyEff[A, U <: this.type](f: R => R !! U)(body: A !! U): A !! U = perform(_.localModifyEff(f)(body))

  /** Predefined handlers for this effect. */
  object handlers:
    def default(initial: R): ThisHandler[Identity, Identity, Any] = ReaderEffect.this.readerHandler(initial)


trait Reader[R] extends ReaderEffect[R]:
  export handlers.{default => handler}

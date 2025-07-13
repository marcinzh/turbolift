package turbolift.effects
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._


trait ReaderSignature[R] extends Signature:
  def ask: R !! ThisEffect
  def asks[A](f: R => A): A !! ThisEffect
  def asksEff[A, U <: ThisEffect](f: R => A !! U): A !! U
  def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !! U
  def localPutEff[A, U <: ThisEffect](r: R !! U)(body: A !! U): A !! U
  def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !! U
  def localModifyEff[A, U <: ThisEffect](f: R => R !! U)(body: A !! U): A !! U


trait ReaderEffect[R] extends Effect[ReaderSignature[R]] with ReaderSignature[R]:
  enclosing =>
  final override val ask: R !! this.type = perform(_.ask)
  final override def asks[A](f: R => A): A !! this.type = perform(_.asks(f))
  final override def asksEff[A, U <: this.type](f: R => A !! U): A !! U = perform(_.asksEff(f))
  final override def localPut[A, U <: this.type](r: R)(body: A !! U): A !! U = perform(_.localPut(r)(body))
  final override def localPutEff[A, U <: this.type](r: R !! U)(body: A !! U): A !! U = perform(_.localPutEff(r)(body))
  final override def localModify[A, U <: this.type](f: R => R)(body: A !! U): A !! U = perform(_.localModify(f)(body))
  final override def localModifyEff[A, U <: this.type](f: R => R !! U)(body: A !! U): A !! U = perform(_.localModifyEff(f)(body))


  /** Predefined handlers for this effect. */
  object handlers:
    def default(initial: R): Handler[Identity, Identity, enclosing.type, Any] =
      new impl.Stateful[Identity, Identity, Any] with impl.Parallel.Trivial with ReaderSignature[R]:
        override type Local = R
        override def onInitial: R !! Any = !!.pure(initial)
        override def onReturn(a: Unknown, r: R): Unknown !! Any = !!.pure(a)

        override val ask: R !! ThisEffect = Local.get
        override def asksEff[A, U <: ThisEffect](f: R => A !! U): A !! U = Local.getsEff(f)
        override def asks[A](f: R => A): A !! ThisEffect = Local.gets(f)
        override def localPut[A, U <: ThisEffect](r: R)(body: A !! U): A !! U = Control.delimitPut(body, r)
        override def localPutEff[A, U <: ThisEffect](r: R !! U)(body: A !! U): A !! U = r.flatMap(Control.delimitPut(body, _))
        override def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !! U = Control.delimitModify(body, f)
        override def localModifyEff[A, U <: ThisEffect](f: R => R !! U)(body: A !! U): A !! U = Local.getsEff(f).flatMap(Control.delimitPut(body, _))
      .toHandler


trait Reader[R] extends ReaderEffect[R]:
  export handlers.{default => handler}

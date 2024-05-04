package turbolift.handlers
import turbolift.!!
import turbolift.effects.{ReaderEffect, ReaderSignature}
import turbolift.Extensions._


extension [R](fx: ReaderEffect[R])
  def readerHandler(initial: R): fx.ThisHandler[Identity, Identity, Any] =
    new fx.impl.Stateful[Identity, Identity, Any] with fx.impl.Parallel.Trivial with ReaderSignature[R]:
      override type Local = R

      override def onInitial: R !! Any = !!.pure(initial)

      override def onReturn(a: Unknown, r: R): Unknown !! Any = !!.pure(a)

      override val ask: R !@! ThisEffect = Local.get

      override def asks[A](f: R => A): A !@! ThisEffect = Local.gets(f)

      override def localPut[A, U <: ThisEffect](r1: R)(body: A !! U): A !@! U = Control.delimitPut(body, r1)

      override def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !@! U = Control.delimitModify(body, f)

    .toHandler

package turbolift.handlers
import turbolift.!!
import turbolift.effects.{ReaderEffect, ReaderSignature}
import turbolift.Extensions._


extension [R](fx: ReaderEffect[R])
  def readerHandler(initial: R): fx.ThisHandler[Identity, Identity, Any] =
    new fx.impl.Stateful[Identity, Identity, Any] with fx.impl.Parallel.Trivial with ReaderSignature[R]:
      override type Stan = R

      override def tailResumptiveHint: Boolean = true

      override def onInitial: R !! Any = !!.pure(initial)

      override def onReturn(a: Unknown, r: R): Unknown !! Any = !!.pure(a)

      override val ask: R !@! ThisEffect = (k, r) => k(r)

      override def asks[A](f: R => A): A !@! ThisEffect = (k, r) => k(f(r))

      override def localPut[A, U <: ThisEffect](r1: R)(body: A !! U): A !@! U = 
        (k, r) => k.local(body, r1).flatMap:
          case (a, k, _) => k(a)

      override def localModify[A, U <: ThisEffect](f: R => R)(body: A !! U): A !@! U =
        (k, r) => k.local(body, f(r)).flatMap:
          case (a, k, _) => k(a)

    .toHandler

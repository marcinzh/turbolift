package turbolift.handlers
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{ChoiceEffect, ChoiceSignature}


extension [Fx <: ChoiceEffect](fx: Fx)
  def choiceHandler_all: fx.ThisHandler[Identity, Vector, Any] =
    new fx.impl.Stateless[Identity, Vector, Any] with fx.impl.Parallel with ChoiceSignature:
      override def onReturn(a: Unknown): Vector[Unknown] !! Any = !!.pure(Vector(a))

      override def onRestart(as: Vector[Unknown]): Unknown !! fx.type = fx.choose(as)

      override def onUnknown(aa: Vector[Unknown]): Option[Unknown] = aa.headOption

      override def onZip[A, B, C](as: Vector[A], bs: Vector[B], k: (A, B) => C): Vector[C] =
        as.flatMap(a => bs.map(b => k(a, b)))

      override def empty: Nothing !! ThisEffect = Control.abort(Vector())

      override def choose[A](as: Iterable[A]): A !! ThisEffect =
        Control.capture(k => as.iterator.flatMapEff(k(_)))

      override def choosePar[A](as: Iterable[A]): A !! ThisEffect =
        Control.capture(k => as.iterator.flatMapParEff(k(_)))

    .toHandler

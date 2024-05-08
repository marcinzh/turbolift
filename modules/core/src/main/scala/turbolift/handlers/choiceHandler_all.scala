package turbolift.handlers
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{ChoiceEffect, ChoiceSignature}


extension (fx: ChoiceEffect)
  def choiceHandler_all: fx.ThisHandler[Identity, Vector, Any] =
    new fx.impl.Stateless[Identity, Vector, Any] with fx.impl.Parallel with ChoiceSignature:
      override def multishotHint: Boolean = true

      override def onReturn(a: Unknown): Vector[Unknown] !! Any = !!.pure(Vector(a))

      override def onRestart(as: Vector[Unknown]): Unknown !! ThisEffect = fx.choose(as)

      override def onZip[A, B, C](as: Vector[A], bs: Vector[B], k: (A, B) => C): Vector[C] =
        as.flatMap(a => bs.map(b => k(a, b)))

      override def empty: Nothing !@! ThisEffect = _ => !!.vector

      override def choose[A](as: Iterable[A]): A !@! ThisEffect =
        k => as.iterator.flatMapEff(k(_))

      //@#@TODO choosePar
      // override def choosePar[A](as: Iterable[A]): A !@! ThisEffect =
      //   k => as.iterator.flatMapParEff(k(_))

    .toHandler

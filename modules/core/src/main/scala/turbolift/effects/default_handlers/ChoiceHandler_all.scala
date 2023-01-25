package turbolift.effects.default_handlers
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{ChoiceEffect, ChoiceSignature}


extension (fx: ChoiceEffect)
  private[effects] def choiceHandler_all: fx.ThisHandler.Free[Vector] =
    new fx.Stateless[Vector] with fx.Parallel with ChoiceSignature:
      override def onPure[A](a: A): Vector[A] = Vector(a)

      override def onUnpure[A](as: Vector[A]): A !! ThisEffect = fx.choose(as)

      override def onZip[A, B, C](as: Vector[A], bs: Vector[B], k: (A, B) => C): Vector[C] =
        as.flatMap(a => bs.map(b => k(a, b)))

      override def fail: Nothing !@! ThisEffect = _ => !!.pure(Vector())

      override def choose[A](as: Iterable[A]): A !@! ThisEffect =
        k => as.iterator.flatMap_!!(k)

    .toHandler

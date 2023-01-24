package turbolift.effects.default_handlers
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{ChoiceEffect, ChoiceSig}


extension (fx: ChoiceEffect)
  private[effects] def choiceHandler_first: fx.ThisHandler.Free[Option] =
    new fx.Stateless[Option] with fx.Parallel with ChoiceSig:
      override def onPure[A](a: A): Option[A] = Some(a)

      override def onUnpure[A](as: Option[A]): A !! ThisEffect =
        as match
          case Some(a) => !!.pure(a)
          case None => fx.fail

      override def onZip[A, B, C](as: Option[A], bs: Option[B], k: (A, B) => C): Option[C] =
        as.flatMap(a => bs.map(b => k(a, b)))

      override def fail: Nothing !@! ThisEffect = _ => !!.pure(None)

      override def choose[A](as: Iterable[A]): A !@! ThisEffect =
        k =>
          val it = as.iterator
          def loop(): Option[Unknown] !! Any =
            if it.hasNext then
              k(it.next()).flatMap {
                case None => loop()
                case x => !!.pure(x)
              }
            else
              !!.pure(None)
          loop()

    .toHandler

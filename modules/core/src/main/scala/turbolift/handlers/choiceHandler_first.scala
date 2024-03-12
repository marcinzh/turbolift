package turbolift.handlers
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{Choice, ChoiceSignature}


extension (fx: Choice)
  def choiceHandler_first: fx.ThisHandler.FromId.Free[Option] =
    new fx.impl.Stateless.FromId.Free[Option] with fx.impl.Parallel with ChoiceSignature:
      override def multishotHint: Boolean = true

      override def onReturn(a: Unknown): Option[Unknown] !! Any = !!.pure(Some(a))

      override def onRestart(as: Option[Unknown]): Unknown !! ThisEffect =
        as match
          case Some(a) => !!.pure(a)
          case None => fx.empty

      override def onZip[A, B, C](as: Option[A], bs: Option[B], k: (A, B) => C): Option[C] =
        as.flatMap(a => bs.map(b => k(a, b)))

      override def empty: Nothing !@! ThisEffect = _ => !!.none

      override def choose[A](as: Iterable[A]): A !@! ThisEffect =
        k =>
          val it = as.iterator
          def loop(): Option[Unknown] !! Ambient =
            if it.hasNext then
              k(it.next()).flatMap {
                case None => loop()
                case x => !!.pure(x)
              }
            else
              !!.none
          loop()

    .toHandler

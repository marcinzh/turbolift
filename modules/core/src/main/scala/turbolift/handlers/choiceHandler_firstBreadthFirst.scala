package turbolift.handlers
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{ChoiceEffect, ChoiceSignature}
import scala.collection.immutable.Queue
import QueO.Cont


extension [Fx <: ChoiceEffect](fx: Fx)
  def choiceHandler_firstBreadthFirst: fx.ThisHandler[Identity, Option, Any] =
    new fx.impl.Stateful[Identity, Option, Any] with fx.impl.Parallel with ChoiceSignature:
      override type Local = QueO[Unknown, ThisEffect]

      override def onInitial = QueO.empty.pure_!!

      override def onReturn(a: Unknown, q: Local): Option[Unknown] !! ThisEffect = !!.pure(Some(a))

      override def onRestart(as: Option[Unknown]): Unknown !! fx.type = fx.fromOption(as)

      override def onUnknown(aa: Option[Unknown]): Option[Unknown] = aa

      override def onZip[A, B, C](as: Option[A], bs: Option[B], k: (A, B) => C): Option[C] =
        as.flatMap(a => bs.map(b => k(a, b)))

      override def empty: Nothing !! ThisEffect =
        Control.captureGet: (_, q) =>
          q.drain

      override def choose[A](as: Iterable[A]): A !! ThisEffect =
        Control.captureGet: (k, q) =>
          q.addTodo(as.iterator.map(a => k(a, _))).drain

      override def choosePar[A](as: Iterable[A]): A !! ThisEffect = choose(as)

    .toHandler


private final case class QueO[A, U](todo: Queue[Cont[A, U]]) extends AnyVal:
  def drain: Option[A] !! U =
    todo.dequeueOption match
      case None => !!.none
      case Some(head, tail) => head(copy(todo = tail))

  def addTodo(xs: IterableOnce[Cont[A, U]]): QueO[A, U] = copy(todo = todo ++ xs)


private object QueO:
  def empty[A, U] = QueO[A, U](Queue())
  type Cont[A, U] = QueO[A, U] => Option[A] !! U

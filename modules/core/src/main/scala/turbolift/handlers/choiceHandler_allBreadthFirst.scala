package turbolift.handlers
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{ChoiceEffect, ChoiceSignature}
import scala.collection.immutable.Queue
import QueV.Cont


extension [Fx <: ChoiceEffect](fx: Fx)
  def choiceHandler_allBreadthFirst: fx.ThisHandler[Identity, Vector, Any] =
    new fx.impl.Stateful[Identity, Vector, Any] with fx.impl.Parallel with ChoiceSignature:
      override type Local = QueV[Unknown, Fx]

      override def onInitial = QueV.empty.pure_!!

      override def onReturn(a: Unknown, q: Local): Vector[Unknown] !! Fx =
        q.addDone(a).drain

      override def onRestart(as: Vector[Unknown]): Unknown !! ThisEffect =
        fx.choose(as)

      override def onZip[A, B, C](as: Vector[A], bs: Vector[B], k: (A, B) => C): Vector[C] =
        as.flatMap(a => bs.map(b => k(a, b)))

      override def empty: Nothing !! ThisEffect =
        Control.captureGet: (_, q) =>
          q.drain

      override def choose[A](as: Iterable[A]): A !! ThisEffect =
        Control.captureGet: (k, q) =>
          q.addTodo(as.iterator.map(a => k(a, _))).drain

      override def choosePar[A](as: Iterable[A]): A !! ThisEffect = choose(as)

    .toHandler


private final case class QueV[A, U](todo: Queue[Cont[A, U]], done: Vector[A]):
  def drain: Vector[A] !! U =
    todo.dequeueOption match
      case None => done.pure_!!
      case Some(head, tail) => head(copy(todo = tail))

  def addTodo(xs: IterableOnce[Cont[A, U]]): QueV[A, U] = copy(todo = todo ++ xs)
  def addDone(a: A): QueV[A, U] = copy(done = done :+ a)


private object QueV:
  def empty[A, U] = QueV[A, U](Queue(), Vector())
  type Cont[A, U] = QueV[A, U] => Vector[A] !! U

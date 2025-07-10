package turbolift.effects
import scala.util.Try
import turbolift.{!!, Signature, Effect, Handler}
import turbolift.Extensions._
import turbolift.data.{QueVector, QueOption}


trait ChoiceSignature extends Signature:
  def empty: Nothing !! ThisEffect
  def choose[A](as: Iterable[A]): A !! ThisEffect
  def choosePar[A](as: Iterable[A]): A !! ThisEffect


trait ChoiceEffect extends Effect[ChoiceSignature] with ChoiceSignature:
  enclosing =>
  final override val empty: Nothing !! this.type = perform(_.empty)
  final override def choose[A](as: Iterable[A]): A !! this.type = perform(_.choose(as))
  final override def choosePar[A](as: Iterable[A]): A !! this.type = perform(_.choosePar(as))

  final def plus[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = choose(Vector(lhs, !!.impureEff(rhs))).flatten
  final def plusPar[A, U <: this.type](lhs: A !! U, rhs: A !! U): A !! U = choosePar(Vector(lhs, rhs)).flatten

  final def apply[A](as: A*): A !! this.type = choose(as.toVector)

  final def fromOption[A](x: Option[A]): A !! this.type = x.fold(empty)(!!.pure)
  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x.fold(_ => empty, !!.pure)
  final def fromTry[A](x: Try[A]): A !! this.type = x.fold(_ => empty, !!.pure)

  /** Predefined handlers for this effect. */
  object handlers:
    /** Collect into a `Vector` all succesful paths in depth-first search order. */
    def all: Handler[Identity, Vector, enclosing.type, Any] =
      new impl.Stateless[Identity, Vector, Any] with impl.Parallel with ChoiceSignature:
        override def onReturn(a: Unknown): Vector[Unknown] !! Any = !!.pure(Vector(a))
        override def onRestart(as: Vector[Unknown]): Unknown !! enclosing.type = enclosing.choose(as)
        override def onUnknown(as: Vector[Unknown]): Option[Unknown] = as.headOption
        override def onZip[A, B, C](as: Vector[A], bs: Vector[B], k: (A, B) => C): Vector[C] = as.flatMap(a => bs.map(k(a, _)))

        override def empty: Nothing !! ThisEffect = Control.abort(Vector())
        override def choose[A](as: Iterable[A]): A !! ThisEffect = Control.capture(k => as.iterator.flatMapEff(k))
        override def choosePar[A](as: Iterable[A]): A !! ThisEffect = Control.capture(k => as.iterator.flatMapParEff(k))
    .toHandler

    /** Collect into an `Option` the first path in depth-first search order. */
    def first: Handler[Identity, Option, enclosing.type, Any] =
      new impl.Stateless[Identity, Option, Any] with impl.Parallel with ChoiceSignature:
        override def onReturn(a: Unknown): Option[Unknown] !! Any = !!.pure(Some(a))
        override def onRestart(as: Option[Unknown]): Unknown !! enclosing.type = as.fold(enclosing.empty)(!!.pure)
        override def onUnknown(as: Option[Unknown]): Option[Unknown] = as
        override def onZip[A, B, C](as: Option[A], bs: Option[B], k: (A, B) => C): Option[C] = as.zip(bs).map(k.tupled)

        override def empty: Nothing !! ThisEffect = Control.abort(None)
        override def choosePar[A](as: Iterable[A]): A !! ThisEffect = choose(as)
        override def choose[A](as: Iterable[A]): A !! ThisEffect =
          Control.capture: k =>
            val it = as.iterator
            def loop(): Option[Unknown] !! ThisEffect =
              if it.hasNext then
                k(it.next()).flatMap:
                  case None => loop()
                  case x => !!.pure(x)
              else
                !!.none
            loop()
      .toHandler

    /** Collect into a `Vector` all succesful paths in breadth-first search order. */
    def allBreadthFirst: Handler[Identity, Vector, enclosing.type, Any] =
      new impl.Stateful[Identity, Vector, Any] with impl.Parallel with ChoiceSignature:
        override type Local = QueVector[Unknown, ThisEffect]
        override def onInitial = QueVector.empty.pure_!!
        override def onReturn(a: Unknown, q: Local): Vector[Unknown] !! ThisEffect = q.addDone(a).drain
        override def onRestart(as: Vector[Unknown]): Unknown !! enclosing.type = enclosing.choose(as)
        override def onUnknown(aa: Vector[Unknown]): Option[Unknown] = aa.headOption
        override def onZip[A, B, C](as: Vector[A], bs: Vector[B], k: (A, B) => C): Vector[C] = as.flatMap(a => bs.map(k(a, _)))

        override def empty: Nothing !! ThisEffect = Control.captureGet((_, q) => q.drain)
        override def choosePar[A](as: Iterable[A]): A !! ThisEffect = choose(as)
        override def choose[A](as: Iterable[A]): A !! ThisEffect =
          Control.captureGet: (k, q) =>
            q.addTodo(as.iterator.map(a => k(a, _))).drain
      .toHandler

    /** Collect into an `Option` the first path in breadth-first search order. */
    def firstBreadthFirst: Handler[Identity, Option, enclosing.type, Any] =
      new impl.Stateful[Identity, Option, Any] with impl.Parallel with ChoiceSignature:
        override type Local = QueOption[Unknown, ThisEffect]
        override def onInitial = QueOption.empty.pure_!!
        override def onReturn(a: Unknown, q: Local): Option[Unknown] !! ThisEffect = !!.pure(Some(a))
        override def onRestart(as: Option[Unknown]): Unknown !! enclosing.type = enclosing.fromOption(as)
        override def onUnknown(aa: Option[Unknown]): Option[Unknown] = aa
        override def onZip[A, B, C](as: Option[A], bs: Option[B], k: (A, B) => C): Option[C] = as.zip(bs).map(k.tupled)

        override def empty: Nothing !! ThisEffect = Control.captureGet((_, q) => q.drain)
        override def choosePar[A](as: Iterable[A]): A !! ThisEffect = choose(as)
        override def choose[A](as: Iterable[A]): A !! ThisEffect =
          Control.captureGet: (k, q) =>
            q.addTodo(as.iterator.map(a => k(a, _))).drain
      .toHandler


trait Choice extends ChoiceEffect:
  export handlers.{all => handler}


/** Predefined instance of [[Choice]] effect. */
case object Each extends Choice:
  def void: ThisHandler[Identity, Const[Unit], Any] = handlers.all.void

type Each = Each.type

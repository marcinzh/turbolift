package turbolift.effects
import scala.util.Try
import turbolift.{!!, Effect, Signature}
import turbolift.handlers.{choiceHandler_first, choiceHandler_all}
import turbolift.handlers.{choiceHandler_allBreadthFirst, choiceHandler_firstBreadthFirst}


trait ChoiceSignature extends Signature:
  def empty: Nothing !@! ThisEffect
  def choose[A](as: Iterable[A]): A !@! ThisEffect


trait Choice extends Effect[ChoiceSignature] with ChoiceSignature:
  final override val empty: Nothing !! this.type = perform(_.empty)
  final override def choose[A](as: Iterable[A]): A !! this.type = perform(_.choose(as))

  final def plus[A, U <: this.type](lhs: A !! U, rhs: => A !! U): A !! U = choose(Vector(lhs, !!.defer(rhs))).flatten

  final def apply[A](as: A*): A !! this.type = choose(as.toVector)

  final def fromOption[A](x: Option[A]): A !! this.type = x.fold(empty)(!!.pure)
  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x.fold(_ => empty, !!.pure)
  final def fromTry[A](x: Try[A]): A !! this.type = x.fold(_ => empty, !!.pure)

  /** Predefined handlers for this effect. */
  object handlers:
    def first: ThisHandler.FromId.Free[Option] = Choice.this.choiceHandler_first
    def all: ThisHandler.FromId.Free[Vector] = Choice.this.choiceHandler_all
    def firstBreadthFirst: ThisHandler.FromId.Free[Option] = Choice.this.choiceHandler_firstBreadthFirst
    def allBreadthFirst: ThisHandler.FromId.Free[Vector] = Choice.this.choiceHandler_allBreadthFirst


object Choice:
  extension (fx: Choice)
    /** Default handler for this effect. */
    def handler: fx.ThisHandler.FromId.Free[Vector] = fx.choiceHandler_all


/** Predefined instance of [[Choice]] effect. */
case object Each extends Choice:
  def void: ThisHandler.FromId.ToConst.Free[Unit] = handlers.all.void

type Each = Each.type

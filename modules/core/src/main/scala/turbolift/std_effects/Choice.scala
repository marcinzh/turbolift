package turbolift.std_effects
import scala.util.{Try, Success}
import turbolift.abstraction.{!!, Effect}


trait ChoiceSig[U] {
  def empty[A]: A !! U
  def plus[A](lhs: A !! U, rhs: => A !! U): A !! U
  def each[A](as: Iterable[A]): A !! U
}


trait ChoiceExt extends Effect[ChoiceSig] {
  final val empty: Nothing !! this.type = embedFO(_.empty)
  final def plus[A, U](lhs: A !! U, rhs: => A !! U): A !! U with this.type = embedHO[U](_.plus(lhs, rhs))
  final def each[A](as: Iterable[A]): A !! this.type = embedFO(_.each(as))

  final def apply[A](as: A*): A !! this.type = each(as.toVector)
  final def fail = empty

  final def fromOption[A](x: Option[A]): A !! this.type = x match {
    case Some(a) => pure(a)
    case _ => empty
  }

  final def fromEither[E, A](x: Either[E, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case _ => empty
  }

  final def fromTry[A](x: Try[A]): A !! this.type = x match {
    case Success(a) => pure(a)
    case _ => empty
  }

  object handlers {
    val one: ThisIHandler[Option] = ChoiceHandler_One(ChoiceExt.this)
    val many: ThisIHandler[Vector] = ChoiceHandler_Many(ChoiceExt.this)
  }
}


trait Choice extends ChoiceExt {
  def handler: ThisIHandler[Vector] = handlers.many
}

trait Fail extends ChoiceExt {
  def handler: ThisIHandler[Option] = handlers.one
}


case object Each extends Choice {
  def void: ThisIHandler[Lambda[X => Unit]] = handlers.many.void
}

trait ChoiceExports {
  type Each = Each.type
}

package turbolift.std_effects
import scala.util.{Try, Success}
import turbolift.abstraction.{!!, Effect}
import turbolift.std_handlers.DefaultChoiceHandler


trait ChoiceSig[U] {
  def empty[A]: A !! U
  def plus[A](lhs: A !! U, rhs: => A !! U): A !! U
  def each[A](as: Iterable[A]): A !! U
}


trait Choice extends Effect[ChoiceSig] {
  final val empty: Nothing !! this.type = embedFO(_.empty)
  final def plus[A, U](lhs: A !! U, rhs: => A !! U): A !! U with this.type = embedHO[U](_.plus(lhs, rhs))
  final def each[A](as: Iterable[A]): A !! this.type = embedFO(_.each(as))

  final def apply[A](as: A*): A !! this.type = each(as.toVector)

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

  val handler: ThisIHandler[Vector] = DefaultChoiceHandler(this)
}

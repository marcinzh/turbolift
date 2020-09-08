package turbolift.std_effects
import scala.util.{Try, Success, Failure}
import cats.Semigroup
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.Accum
import turbolift.std_handlers.DefaultValidationHandler


trait ValidationSig[U, E] {
  def invalid[A](e: E): A !! U
  def validate[A](scope: A !! U)(recover: E => A !! U): A !! U
}


trait Validation[E] extends Effect[ValidationSig[?, E]] {
  final def invalid(e: E): Nothing !! this.type = embedFO(_.invalid(e))
  final def invalid[X](x: X)(implicit ev: Accum[X, E]): Nothing !! this.type = invalid(ev.one(x))
  final def validate[A, U](scope: A !! U)(recover: E => A !! U): A !! U with this.type = embedHO[U](_.validate(scope)(recover))

  final def fromEither[A](x: Either[E, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => invalid(e)
  }

  final def fromOption[A](x: Option[A])(e : => E): A !! this.type = x match {
    case Some(a) => pure(a)
    case _ => invalid(e)
  }

  final def fromTry[A](x: Try[A])(implicit ev: E <:< Throwable): A !! this.type = x match {
    case Success(a) => pure(a)
    case Failure(e) => invalid(e.asInstanceOf[E])
  }

  def handler(implicit E: Semigroup[E]): ThisIHandler[Either[E, ?]] = DefaultValidationHandler[E, this.type](this)
}

package turbolift.std_effects
import scala.util.{Try, Success, Failure}
import cats.Semigroup
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.Accum
import turbolift.std_handlers.DefaultValidationHandler


trait ValidationExtSig[U, E, E1] {
  def invalid[A](e: E1): A !! U
  def invalids[A](e: E): A !! U
  def validate[A](scope: A !! U)(recover: E => A !! U): A !! U
}


trait ValidationExt[E, E1] extends Effect[ValidationExtSig[?, E, E1]] {
  final def invalid(e: E1): Nothing !! this.type = embedFO(_.invalid(e))
  final def invalids(e: E): Nothing !! this.type = embedFO(_.invalids(e))
  final def validate[A, U](scope: A !! U)(recover: E => A !! U): A !! U with this.type = embedHO[U](_.validate(scope)(recover))

  final def fromEither[A](x: Either[E1, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => invalid(e)
  }

  final def fromOption[A](x: Option[A])(e : => E1): A !! this.type = x match {
    case Some(a) => pure(a)
    case _ => invalid(e)
  }

  final def fromTry[A](x: Try[A])(implicit ev: E1 <:< Throwable): A !! this.type = x match {
    case Success(a) => pure(a)
    case Failure(e) => invalid(e.asInstanceOf[E1])
  }

  def handler(implicit E: Accum[E, E1]): ThisIHandler[Either[E, ?]] = DefaultValidationHandler[E, E1, this.type](this)
}


trait Validation[E] extends ValidationExt[E, E]

trait ValidationK[E, F[_]] extends ValidationExt[E, F[E]]

trait ValidationExports {
  type ValidationSig[U, E] = ValidationExtSig[U, E, E]

  type ValidationKSig[U, F[_], E] = ValidationExtSig[U, F[E], E]
}

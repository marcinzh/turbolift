package turbolift.std_effects
import scala.util.{Try, Success, Failure}
import cats.Semigroup
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.Accum
import turbolift.std_handlers.DefaultValidationHandler


trait ValidationExtSig[U, E, EE] {
  def invalid[A](e: E): A !! U
  def invalids[A](ee: EE): A !! U
  def validate[A](scope: A !! U)(recover: EE => A !! U): A !! U
}


trait ValidationExt[E, EE] extends Effect[ValidationExtSig[?, E, EE]] {
  final def invalid(e: E): Nothing !! this.type = embedFO(_.invalid(e))
  final def invalids(ee: EE): Nothing !! this.type = embedFO(_.invalids(ee))
  final def validate[A, U](scope: A !! U)(recover: EE => A !! U): A !! U with this.type = embedHO[U](_.validate(scope)(recover))

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

  def handler(implicit E: Accum[E, EE]): ThisIHandler[Either[EE, ?]] = DefaultValidationHandler[E, EE, this.type](this)
}


trait Validation[E] extends ValidationExt[E, E]

trait ValidationK[E, F[_]] extends ValidationExt[E, F[E]]

trait ValidationExports {
  type ValidationSig[U, E] = ValidationExtSig[U, E, E]

  type ValidationKSig[U, E, F[_]] = ValidationExtSig[U, E, F[E]]
}

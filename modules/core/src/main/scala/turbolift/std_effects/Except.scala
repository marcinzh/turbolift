package turbolift.std_effects
import scala.util.{Try, Success, Failure}
import turbolift.abstraction.{!!, Effect}
import turbolift.std_handlers.DefaultExceptHandler


trait ExceptSig[U, E] {
  def raise[A](e: E): A !! U
  def katch[A](scope: A !! U)(recover: E => A !! U): A !! U
}


trait Except[E] extends Effect[ExceptSig[?, E]] {
  final def raise(e: E): Nothing !! this.type = embedFO(_.raise(e))
  final def katch[A, U](scope: A !! U)(recover: E => A !! U): A !! U with this.type = embedHO[U](_.katch(scope)(recover))

  final def fromEither[A](x: Either[E, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => raise(e)
  }

  final def fromOption[A](x: Option[A])(e : => E): A !! this.type = x match {
    case Some(a) => pure(a)
    case _ => raise(e)
  }

  final def fromTry[A](x: Try[A])(implicit ev: E <:< Throwable): A !! this.type = x match {
    case Success(a) => pure(a)
    case Failure(e) => raise(e.asInstanceOf[E])
  }

  val handler: ThisHandler[Either[E, ?]] = DefaultExceptHandler[E, this.type](this)
}

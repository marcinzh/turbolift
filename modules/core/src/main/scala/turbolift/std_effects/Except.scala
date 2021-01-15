package turbolift.std_effects
import scala.util.{Try, Success, Failure}
import turbolift.abstraction.{!!, Effect}
import turbolift.abstraction.typeclass.Accum


trait ExceptExtSig[U, E, E1] {
  def raise[A](e: E1): A !! U
  def raises[A](e: E): A !! U
  def katch[A](scope: A !! U)(fun: E => A !! U): A !! U
}


trait ExceptExt[E, E1] extends Effect[ExceptExtSig[*, E, E1]] {
  final def raise(e: E1): Nothing !! this.type = embedFO(_.raise(e))
  final def raises(e: E): Nothing !! this.type = embedFO(_.raises(e))
  final def katch[A, U](scope: A !! U)(fun: E => A !! U): A !! U with this.type = embedHO[U](_.katch(scope)(fun))

  final def fromEither[A](x: Either[E1, A]): A !! this.type = x match {
    case Right(a) => pure(a)
    case Left(e) => raise(e)
  }

  final def fromOption[A](x: Option[A])(e : => E1): A !! this.type = x match {
    case Some(a) => pure(a)
    case _ => raise(e)
  }

  final def fromTry[A](x: Try[A])(implicit ev: E1 <:< Throwable): A !! this.type = x match {
    case Success(a) => pure(a)
    case Failure(e) => raise(e.asInstanceOf[E1])
  }

  object handlers {
    def one(implicit E: E1 =:= E): ThisIHandler[Either[E, *]] = ExceptHandler_One[E, E1, ExceptExt.this.type](ExceptExt.this)
    def many(implicit E: Accum[E, E1]): ThisIHandler[Either[E, *]] = ExceptHandler_Many[E, E1, ExceptExt.this.type](ExceptExt.this)
  }
}


trait Except[E] extends ExceptExt[E, E] {
  def handler = handlers.one
}

trait ExceptK[F[_], E] extends ExceptExt[F[E], E]

trait Validation[E] extends ExceptExt[E, E] {
  def handler(implicit E: Accum[E, E]) = handlers.many
}

trait ValidationK[F[_], E] extends ExceptExt[F[E], E] {
  def handler(implicit E: Accum[F[E], E]) = handlers.many
}

trait ExceptExports {
  type ExceptSig[U, E] = ExceptExtSig[U, E, E]

  type ExceptKSig[U, F[_], E] = ExceptExtSig[U, F[E], E]
}

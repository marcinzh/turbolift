package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.{Outcome, Cause}
import turbolift.data.Exceptions.Unhandled


class ExceptionTest extends Specification:
  case object E extends Exception { override def toString = productPrefix }

  def bad1 = !!.impure(throw E)
  def bad2 = IO.sync(throw E)
  def bad3 = IO.raise(E)

  "throw" >> {
    "impure(throw)"  >>{bad1.runIO match { case Outcome.Failure(Cause.Thrown(_: Unhandled)) => success; case x => failure(x.toString) }}
    "IO.sync(throw)" >>{bad2.runIO === Outcome.Failure(Cause.Thrown(E)) }
    "IO.raise"       >>{bad3.runIO === Outcome.Failure(Cause.Thrown(E)) }
  }

  "catchToEither" >> {
    "pure"           >>{IO.catchToEither(!!.pure(42)).runIO === Outcome.Success(Right(42)) }
    "IO.sync(throw)" >>{IO.catchToEither(bad2).runIO === Outcome.Success(Left(E)) }
    "IO.raise"       >>{IO.catchToEither(bad3).runIO === Outcome.Success(Left(E)) }
  }

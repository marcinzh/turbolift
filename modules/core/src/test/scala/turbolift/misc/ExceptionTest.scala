package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, ErrorEffect}
import turbolift.data.{Outcome, Cause}
import turbolift.data.Exceptions.Unhandled


class ExceptionTest extends Specification:
  "basic" >> {
    case object E extends Exception { override def toString = productPrefix }

    def bad1 = !!.impure(throw E)
    def bad2 = IO.sync(throw E)
    def bad3 = IO.raise(E)

    "throw" >> {
      "impure(throw)"  >>{bad1.runSync match { case Outcome.Failure(Cause.Thrown(_: Unhandled)) => success; case x => failure(x.toString) }}
      "IO.sync(throw)" >>{bad2.runSync === Outcome.Failure(Cause.Thrown(E)) }
      "IO.raise"       >>{bad3.runSync === Outcome.Failure(Cause.Thrown(E)) }
    }

    "catchToEither" >> {
      "pure"           >>{IO.catchToEither(!!.pure(42)).runSync === Outcome.Success(Right(42)) }
      "IO.sync(throw)" >>{IO.catchToEither(bad2).runSync === Outcome.Success(Left(E)) }
      "IO.raise"       >>{IO.catchToEither(bad3).runSync === Outcome.Success(Left(E)) }
    }
  }

  "chained Cause" >> {
    case object E extends ErrorEffect[Unit]
    case object E1 extends Exception { override val toString = productPrefix }
    case object E2 extends Exception { override val toString = productPrefix }
    val C1 = Cause.Thrown(E1)
    val C2 = Cause.Thrown(E2)
    val C12 = C1 && C2

    "accumulate" >> {
      "IO.raise" >>{
        IO.raise(E1).guarantee(IO.raise(E2)).runSync === Outcome.Failure(C12)
      }

      "IO.sync(throw)" >>{
        IO.sync(throw E1).guarantee(IO.sync(throw E2)).runSync === Outcome.Failure(C12)
      }

      "Error.raise" >>{
        (E.raise(()).guarantee(IO.raise(E2)).handleWith(E.handler).runSync: @unchecked) match
          case Outcome.Failure(Cause.Then(Cause.Aborted(Left(()), _), C2)) => success
          case o => failure(o.toString)
      }
    }

    "reset" >> {
      "catchToEither" >>{
        IO.catchToEither(IO.raise(E1)).guarantee(IO.raise(E2)).runSync === Outcome.Failure(C2)
      }
    }
  }

  "conversion between Error & Exception" >> {
    case object X extends Exception { override val toString = productPrefix }
    case object E extends ErrorEffect[Throwable]
    case object F extends ErrorEffect[String]

    "exception -> error" >>{
      IO.raise(X).raiseOnException(E).as(42)
      .handleWith(E.handler)
      .runSync === Outcome.Success(Left(X))
    }

    "error -> exception" >>{
      E.raise(X).throwOnError(E).as(42)
      .handleWith(E.handler)
      .runSync === Outcome.Failure(X)
    }

    "error -> cancellation" >>{
      F.raise("OMG").cancelOnError(F).as(42)
      .handleWith(F.handler)
      .runSync === Outcome.Cancelled
    }

    "handler: error -> exception" >>{
      E.raise(X).as(42)
      .handleWith(E.handler.getOrElseThrow)
      .runSync === Outcome.Failure(X)
    }

    "handler: error -> cancellation" >>{
      F.raise("OMG").as(42)
      .handleWith(F.handler.getOrElseCancel)
      .runSync === Outcome.Cancelled
    }

    "race with Errors" >>{
      val a = IO.sleep(10) &&! F.raise("OMG").as(42)
      val b = IO.sleep(20) &&! !!.pure(1337)

      "without cancelOnError" >>{
        (a |! b)
        .handleWith(F.handler)
        .runSync === Outcome.Success(Left("OMG"))
      }

      "with cancelOnError" >>{
        (a.cancelOnError(F) |! b)
        .handleWith(F.handler)
        .runSync === Outcome.Success(Right(1337))
      }
    }
  }

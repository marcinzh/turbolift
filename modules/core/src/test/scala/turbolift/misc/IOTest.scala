package turbolift.misc
import java.util.concurrent.{Executor => JExecutor}
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, Error}
import turbolift.io.{Outcome, Cause}
import turbolift.internals.executor.Executor
import turbolift.mode.ST


class IOTest extends Specification:
  "Basic ops" >> {
    "raise" >>{
      val e = new Exception("e")
      IO.raise(e).unsafeRun === Outcome.Failure(Cause.Thrown(e))
    }

    "cancel" >>{
      IO.cancel.unsafeRun === Outcome.Cancelled
    }

    "yield" >>{
      IO.yeld.unsafeRun === Outcome.Success(())
    }

    "yield order" >>{
      var accum = ""
      def append(s: String) = !!.impure { accum = accum ++ s }
      val prog1 =
        for
          _ <- append("1")
          _ <- IO.yeld
          _ <- append("2")
        yield ()
      val prog2 = append("3")

      (prog1 &! prog2).unsafeRun
      accum === "132"
    }
  }


  "blocking" >> {
    case object E extends Exception

    "success" >>{
      IO.blocking(42).runIO.===(Outcome.Success(42))
    }

    "failure" >>{
      IO.blocking(throw E).runIO.===(Outcome.Failure(Cause.Thrown(E)))
    }

    "attempt success" >>{
      IO.blockingAttempt(42).runIO === Outcome.Success(Right(42))
    }

    "attempt failure" >>{
      IO.blockingAttempt(throw E).runIO === Outcome.Success(Left(E))
    }
  }


  "executeOn" >> {
    val otherExec = Executor.fromScala(scala.concurrent.ExecutionContext.global)

    "basic" >>{
      IO.executeOn(otherExec)(!!.pure(42))
      .runIO
      .===(Outcome.Success(42))
    }

    "unwind" >> {
      "success" >>{
        (for
          ex1 <- IO.executor
          ex2 <- IO.executeOn(otherExec)(IO.executor)
          ex3 <- IO.executor
        yield (ex1 == ex3, ex2 == otherExec))
        .runIO
        .===(Outcome.Success((true, true)))
      }

      "error" >>{
        case object E extends Error[String]
        (for
          ex1 <- IO.executor
          err <- E.raise("OMG").executeOn(otherExec).handleWith(E.handler)
          ex2 <- IO.executor
        yield (ex1 == ex2, err))
        .runIO
        .===(Outcome.Success((true, Left("OMG"))))
      }

      "exception" >>{
        case object E extends Exception
        (for
          ex1 <- IO.executor
          err <- IO.toEither(IO.raise(E).executeOn(otherExec))
          ex2 <- IO.executor
        yield (ex1 == ex2, err))
        .runIO
        .===(Outcome.Success((true, Left(E))))
      }
    }
  }

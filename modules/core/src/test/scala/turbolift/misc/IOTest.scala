package turbolift.misc
import java.util.concurrent.{Executor => JExecutor}
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, Error}
import turbolift.io.{AtomicVar, Outcome, Cause}
import turbolift.internals.executor.Executor
import Auxx._


class IOTest extends Specification:
  sequential

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
      import turbolift.mode.ST
      (for
        v <- AtomicVar(0)
        prog1 =
          for
            _ <- v.event(1)
            _ <- IO.yeld
            _ <- v.event(2)
          yield ()
        prog2 = v.event(3)
        _ <- prog1 &! prog2
        n <- v.get
      yield n)
      .runIO.===(Outcome.Success(132))
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

    "fork & cancel " >>{
      @volatile var x: Int = 42
      (for
        fib <- IO.blocking { Thread.sleep(1000); x = 1337 }.fork
        _ <- IO.sleep(10)
      yield x)
      .warpCancel
      .runIO
      .===(Outcome.Success(42))
    }
  }


  "async" >> {
    case object E extends Exception

    "success" >>{
      @volatile var x: Int = -1
      (for
        a <- IO.async: cb =>
          x = 42
          cb(Right("omg"))
        b <- IO(x)
      yield (a, b))
      .runIO
      .===(Outcome.Success(("omg", 42)))
    }

    "failure" >>{
      IO.async: cb =>
        cb(Left(E))
      .runIO
      .===(Outcome.Failure(Cause.Thrown(E)))
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

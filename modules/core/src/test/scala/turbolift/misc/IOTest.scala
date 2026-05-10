package turbolift.misc
import java.util.concurrent.{Executor => JExecutor}
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, ErrorEffect}
import turbolift.data.{Outcome, Cause}
import turbolift.io.{AtomicVar, OnceVar}
import turbolift.internals.executor.Executor
import Auxx._


class IOTest extends Specification:
  sequential

  "Basic ops" >> {
    "raise" >>{
      val e = new Exception("e")
      IO.raise(e).runSync === Outcome.Failure(Cause.Thrown(e))
    }

    "cancel" >>{
      IO.cancel.runSync === Outcome.Cancelled
    }

    "yield" >>{
      IO.yeld.runSync === Outcome.Success(())
    }

    "yield order" >>{
      import turbolift.runtime.ST
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
      .runSync === Outcome.Success(132)
    }
  }


  "blocking" >> {
    case object E extends Exception

    "success" >>{
      IO.blocking(42).runSync === Outcome.Success(42)
    }

    "failure" >>{
      IO.blocking(throw E).runSync === Outcome.Failure(Cause.Thrown(E))
    }

    "attempt success" >>{
      IO.attemptBlocking(42).runSync === Outcome.Success(Right(42))
    }

    "attempt failure" >>{
      IO.attemptBlocking(throw E).runSync === Outcome.Success(Left(E))
    }

    "fork & cancel " >>{
      @volatile var x: Int = 42
      (for
        fib <- IO.blocking { Thread.sleep(1000); x = 1337 }.fork
        _ <- IO.sleep(10)
      yield x)
      .warpCancel
      .runSync === Outcome.Success(42)
    }

    "fork & Thread.interrupt" >>{
      (for
        ovar <- OnceVar[Thread]
        fib <-
          IO.blocking:
            ovar.unsafePut(Thread.currentThread().nn)
            Thread.sleep(1000)
            42
          .fork
        thread <- ovar.get
        _ <- IO.sleep(10)
        _ <- IO.sync(thread.interrupt())
        zipp <- fib.await
        outcome = zipp.getIO
      yield outcome)
      .warp
      .runSync match
        case Outcome.Success(Outcome.Failure(Cause.Thrown(_: InterruptedException))) => success
        case x => failure(x.toString)
    }
  }


  "async" >> {
    case object E extends Exception

    "plain" >> {
      "success" >>{
        @volatile var x: Int = -1
        (for
          a <- IO.async: cb =>
            x = 42
            cb(Right("omg"))
          b <- IO(x)
        yield (a, b))
        .runSync === Outcome.Success(("omg", 42))
      }

      "failure" >>{
        IO.async: cb =>
          cb(Left(E))
        .runSync === Outcome.Failure(Cause.Thrown(E))
      }
    }

    "attempt" >> {
      "success" >>{
        @volatile var x: Int = -1
        (for
          a <- IO.attemptAsync: cb =>
            x = 42
            cb(Right("omg"))
          b <- IO(x)
        yield (a, b))
        .runSync === Outcome.Success((Right("omg"), 42))
      }

      "failure" >>{
        IO.attemptAsync: cb =>
          cb(Left(E))
        .runSync === Outcome.Success(Left(E))
      }
    }
  }



  "executeOn" >> {
    val otherExec = Executor.fromScala(scala.concurrent.ExecutionContext.global)

    "basic" >>{
      IO.executeOn(otherExec)(!!.pure(42))
      .runSync === Outcome.Success(42)
    }

    "unwind" >> {
      "success" >>{
        (for
          ex1 <- IO.executor
          ex2 <- IO.executeOn(otherExec)(IO.executor)
          ex3 <- IO.executor
        yield (ex1 == ex3, ex2 == otherExec))
        .runSync === Outcome.Success((true, true))
      }

      "error" >>{
        case object E extends ErrorEffect[String]
        (for
          ex1 <- IO.executor
          err <- E.raise("OMG").executeOn(otherExec).handleWith(E.handler)
          ex2 <- IO.executor
        yield (ex1 == ex2, err))
        .runSync === Outcome.Success((true, Left("OMG")))
      }

      "exception" >>{
        case object E extends Exception
        (for
          ex1 <- IO.executor
          err <- IO.catchToEither(IO.raise(E).executeOn(otherExec))
          ex2 <- IO.executor
        yield (ex1 == ex2, err))
        .runSync === Outcome.Success((true, Left(E)))
      }
    }
  }

package turbolift.misc
import org.specs2.mutable._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import turbolift.!!
import turbolift.effects._
import turbolift.data.Outcome
import turbolift.io.{Fiber, Warp, OnceVar}


class ZipperTest extends Specification:
  sequential

  "Basic ops" >> {
    "reify" >> {
      "run" >>{
        (for
          fib <- !!.pure(42).fork
          zip <- fib.await
          a <- zip.run
        yield a)
        .warp
        .runIO
        .===(Outcome.Success(42))
      }

      "handleIO + get" >>{
        (for
          fib <- IO(42).fork
          zip <- fib.await
          a = zip.handleIO.map(_.get)
        yield a)
        .warp
        .runIO
        .===(Outcome.Success(Outcome.Success(42)))
      }

      "get" >>{
        (for
          fib <- !!.pure(42).fork
          zip <- fib.await
          a = zip.get
        yield a)
        .warp
        .runIO
        .===(Outcome.Success(42))
      }

      "getIO" >>{
        (for
          fib <- IO(42).fork
          zip <- fib.await
          oa = zip.getIO
        yield oa)
        .warp
        .runIO
        .===(Outcome.Success(Outcome.Success(42)))
      }
    }
  }


  "Other" >> {
    "already cancelled" >>{
      (for
        fib <- IO.cancel.fork
        _ <- IO.sleep(10)
        zip <- fib.await
      yield zip.outcome)
      .warp
      .runIO
      .===(Outcome.Success(Outcome.Cancelled))
    }

    "implicit fiber" >>{
      (for
        ovar <- OnceVar[Fiber.Untyped]
        _ <- (IO.sleep(10) *! Fiber.current.flatMap(ovar.put)).fork
        fib <- ovar.get
        zip <- fib.await
      yield zip.outcome)
      .warp
      .runIO
      .===(Outcome.Success(Outcome.Success(())))
    }
  }



  "effectful" >> {
    "with Error" >>{
      case object E extends ErrorK[List, String]
      (for
        fib1 <- E.raise("A").fork
        fib2 <- E.raise("B").fork
        zip1 <- fib1.await
        zip2 <- fib2.await
        zip3 = zip1 *! zip2
        a <- zip3.run
      yield a)
      .handleWith(E.handlers.all)
      .warp
      .runIO
      .===(Outcome.Success(Left(List("A", "B"))))
    }

    "with Writer" >>{
      case object W extends WriterK[List, String]
      (for
        fib1 <- W.tell("A").fork
        fib2 <- W.tell("B").fork
        zip1 <- fib1.await
        zip2 <- fib2.await
        zip3 = zip1 *! zip2
        _ <- W.tell("1")
        a <- zip3.run
        _ <- W.tell("2")
      yield a)
      .handleWith(W.handler.justState)
      .warp
      .runIO
      .===(Outcome.Success(List("1", "A", "B", "2")))
    }
  }


  "type safety" >> {
    "get requires empty effect" >>{
      Typecheck {"""
        for
          fib <- IO(42).fork
          zip <- fib.await
          a = zip.get
        yield a
      """} must succeed.not
    }

    "getIO requires IO-only effect" >>{
      Typecheck {"""
        case object S extends State[Unit];
        for
          fib <- S.put(()).fork
          zip <- fib.await
          oa = zip.getIO
        yield oa
      """} must succeed.not
    }
  }

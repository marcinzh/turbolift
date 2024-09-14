package turbolift.misc
import java.util.concurrent.CountDownLatch
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, Error}
import turbolift.io.{Fiber, Warp, Outcome, AtomicVar, OnceVar}
import Auxx._


class WarpTest extends Specification:
  sequential

  "empty scoped warp" >>{
    !!.pure(42)
    .warp
    .runIO
    .===(Outcome.Success(42))
  }

  "status" >> {
    "child list" >>{
      (for
        g <- Gate(1)
        warp0 <- Warp.current
        warp1 <- warp0.spawn
        fib1 <- g.enter.forkAt(warp1)
        fib2 <- g.enter.forkAt(warp1)
        warp2 <- warp1.spawn
        a <- warp1.children.map(_.size)
        b <- warp1.fibers.map(_.size)
        c <- warp1.warps.map(_.size)
        _ <- g.open
      yield (a, b, c))
      .warp
      .runIO
      .===(Outcome.Success((3, 2, 1)))
    }
  }

  "child & parent" >> {
    "global warp's parent" >>{
      Warp.root.parent === None
    }

    "scoped warp's parent" >>{
      (for
        fib <- Fiber.current
        warp <- Warp.current
        ok = warp.parent == Some(fib)
      yield ok)
      .warp
      .runIO
      .===(Outcome.Success(true))
    }

    "forked fiber's parent" >>{
      (for
        warp <- Warp.current
        fib <- !!.unit.fork
        ok = warp == fib.parent 
      yield ok)
      .warp
      .runIO
      .===(Outcome.Success(true))
    }

    "2 nested scoped warps" >>{
      (for
        warp1 <- Warp.current
        warp2 <- Warp.current.warp
        ok = warp2.outer == Some(warp1)
      yield ok)
      .warp
      .runIO
      .===(Outcome.Success(true))
    }
  }

  "awaiting & cancelling" >> {
    "scoped warp & automatic cancel" >>{
      (for
        v <- AtomicVar(1)
        g <- Gate(1)
        _ <-
          (for
            fib <- (g.open &&! IO.sleep(100) &&! v.event(2)).guarantee(v.event(5)).fork
            _ <- g.enter
          yield ())
          .warpCancel
        _ <- v.event(3)
        a <- v.get
      yield a)
      .runIO
      .===(Outcome.Success(153))
    }

    "scoped warp & automatic shutdown" >>{
      (for
        v <- AtomicVar(1)
        _ <- (IO.sleep(100) &&! v.event(2)).fork.warpAwait
        _ <- v.event(3)
        a <- v.get
      yield a)
      .runIO
      .===(Outcome.Success(123))
    }

    "unscoped warp & manual cancel" >>{
      (for
        v <- AtomicVar(1)
        g <- Gate(1)
        warp <- Warp.unscoped
        _ <- warp.fork:
          for
            _ <- v.event(2)
            _ <- g.open
            _ <- IO.sleep(100)
            _ <- v.event(3)
          yield ()
        _ <- g.enter
        _ <- v.event(4)
        _ <- warp.cancel
        a <- v.get
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(124))
    }

    "unscoped warp & manual shutdown" >>{
      (for
        g <- Gate(1)
        v1 <- AtomicVar(1)
        v2 <- AtomicVar("a")
        warp <- Warp.unscoped
        _ <- (g.enter &&! v1.put(2)).forkAt(warp)
        _ <- (g.enter &&! v2.put("b")).forkAt(warp)
        _ <- g.open
        _ <- warp.await
        a <- v1.get
        b <- v2.get
      yield (a, b))
      .warp
      .runIO
      .===(Outcome.Success((2, "b")))
    }
  }

  "unwind in scoped warp" >> {
    "exception" >>{
      case object E extends Exception
      IO(throw E)
      .warp
      .runIO
      .===(Outcome.Failure(E))
    }

    "Error effect" >>{
      case object E extends Error[String]
      E.raise("OMG")
      .warp
      .handleWith(E.handler)
      .runIO
      .===(Outcome.Success(Left("OMG")))
    }

    "Error effect & paused fiber" >>{
      case object E extends Error[String]
      (for
        v <- AtomicVar(42)
        e <- 
          (for
            _ <- (IO.sleep(1000) &&! v.put(1337)).fork
            _ <- E.raise("OMG")
          yield ())
          .warp
          .handleWith(E.handler)
        a <- v.get
      yield (a, e))
      .runIO
      .===(Outcome.Success((42, Left("OMG"))))
    }

    "Error effect & paused fiber with guarantee" >>{
      case object E extends Error[String]
      (for
        v1 <- AtomicVar(42)
        v2 <- AtomicVar("a")
        g <- Gate(1)
        e <- 
          (for
            _ <- (g.open &&! IO.sleep(1000) &&! v1.put(1337)).guarantee(v2.put("b")).fork
            _ <- g.enter
            _ <- E.raise("OMG")
          yield ())
          .warp
          .handleWith(E.handler)
        a <- v1.get
        b <- v2.get
      yield (a, b, e))
      .runIO
      .===(Outcome.Success((42, "b", Left("OMG"))))
    }
  }

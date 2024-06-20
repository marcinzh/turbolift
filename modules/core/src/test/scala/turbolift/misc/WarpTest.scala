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
    Warp:
      !!.pure(42)
    .runIO
    .===(Outcome.Success(42))
  }

  "status" >> {
    "child list" >>{
      (for
        g <- Gate(1)
        warp <- Warp.spawn
        fib1 <- g.enter.forkAt(warp)
        fib2 <- g.enter.forkAt(warp)
        warp2 <- warp.spawn
        a <- warp.children.map(_.size)
        b <- warp.fibers.map(_.size)
        c <- warp.warps.map(_.size)
        _ <- g.open
      yield (a, b, c))
      .runIO
      .===(Outcome.Success((3, 2, 1)))
    }
  }

  "child & parent" >> {
    "initial warp is current warp" >>{
      (for
        warp1 <- Warp.current
        warp2 <- Warp.initial
        ok = warp1 == warp2
      yield ok)
      .runIO
      .===(Outcome.Success(true))
    }

    "initial warp's parent" >>{
      (for
        warp2 <- Warp.current
        warp1 = Warp.root
        ok = warp1 == warp2.parent
      yield ok)
      .runIO
      .===(Outcome.Success(true))
    }

    "initial fiber's parent" >>{
      (for
        warp <- Warp.current
        fib <- Fiber.current
        ok = fib.parent == warp
      yield ok)
      .runIO
      .===(Outcome.Success(true))
    }

    "forked fiber's parent" >>{
      (for
        warp <- Warp.current
        fib <- !!.unit.fork
        ok = warp == fib.parent 
      yield ok)
      .runIO
      .===(Outcome.Success(true))
    }

    "scoped warp's parent" >>{
      (for
        warp1 <- Warp.current
        warp2 <-
          Warp:
            Warp.current
        ok = warp2.parent == warp1
      yield ok)
      .runIO
      .===(Outcome.Success(true))
    }
  }

  "awaiting & cancelling" >> {
    "initial warp on exit cancels child fiber" >>{
      @volatile var v = 42
      (for
        g <- Gate(1)
        _ <- (g.open &&! IO.sleep(100)).guarantee(IO { v = 1337 }).fork
        _ <- g.enter
      yield v)
      .runIO
      .map((_, v))
      .===(Outcome.Success((42, 1337)))
    }

    "scoped warp on exit cancels child fiber" >>{
      (for
        v <- AtomicVar.fresh(1)
        _ <-
          Warp:
            (IO.sleep(100) &&! v.event(2)).fork
        _ <- v.event(3)
        a <- v.get
      yield a)
      .runIO
      .===(Outcome.Success(13))
    }

    "spawn & cancel" >>{
      (for
        v <- AtomicVar.fresh(1)
        g <- Gate(1)
        warp <- Warp.spawn
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
      .runIO
      .===(Outcome.Success(124))
    }

    "spawn & shutdown" >>{
      (for
        g <- Gate(1)
        v1 <- AtomicVar.fresh(1)
        v2 <- AtomicVar.fresh("a")
        warp <- Warp.spawn
        _ <- (g.enter &&! v1.put(2)).forkAt(warp)
        _ <- (g.enter &&! v2.put("b")).forkAt(warp)
        _ <- g.open
        _ <- warp.shutdown
        a <- v1.get
        b <- v2.get
      yield (a, b))
      .runIO
      .===(Outcome.Success((2, "b")))
    }
  }

  "unwind in scoped warp" >> {
    "exception" >>{
      case object E extends Exception
      Warp:
        IO(throw E)
      .runIO
      .===(Outcome.Failure(E))
    }

    "Error effect" >>{
      case object E extends Error[String]
      Warp:
        E.raise("OMG")
      .handleWith(E.handler)
      .runIO
      .===(Outcome.Success(Left("OMG")))
    }

    "Error effect & paused fiber" >>{
      case object E extends Error[String]
      (for
        v <- AtomicVar.fresh(42)
        e <- 
          Warp:
            (for
              _ <- (IO.sleep(1000) &&! v.put(1337)).fork
              _ <- E.raise("OMG")
            yield ())
          .handleWith(E.handler)
        a <- v.get
      yield (a, e))
      .runIO
      .===(Outcome.Success((42, Left("OMG"))))
    }

    "Error effect & paused fiber with guarantee" >>{
      case object E extends Error[String]
      (for
        v1 <- AtomicVar.fresh(42)
        v2 <- AtomicVar.fresh("a")
        g <- Gate(1)
        e <- 
          Warp:
            (for
              _ <- (g.open &&! IO.sleep(1000) &&! v1.put(1337)).guarantee(v2.put("b")).fork
              _ <- g.enter
              _ <- E.raise("OMG")
            yield ())
          .handleWith(E.handler)
        a <- v1.get
        b <- v2.get
      yield (a, b, e))
      .runIO
      .===(Outcome.Success((42, "b", Left("OMG"))))
    }
  }

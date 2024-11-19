package turbolift.misc
import org.specs2.mutable._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import turbolift.!!
import turbolift.effects._
import turbolift.io.{Fiber, Outcome, Cause, AtomicVar}
import Auxx._


class FiberTest extends Specification:
  sequential

  "named syntax" >> {
    "main" >>{
      (for
        fib <- Fiber.current
      yield fib.name)
      .named("main").runIO
      .===(Outcome.Success("main"))
    }

    "forks" >>{
      (for
        fib1 <- !!.unit.named("fork 1").fork
        fib2 <- !!.unit.named("fork 2").fork
      yield (fib1.name, fib2.name))
      .warp
      .runIO
      .===(Outcome.Success(("fork 1", "fork 2")))
    }
  }

  //---------------------------------------------------------

  "status" >> {
    "runner" >>{
      (for
        fib <- Fiber.current
        s <- fib.status 
      yield s)
      .runIO
      .===(Outcome.Success(Fiber.Status.Pending(Fiber.Role.Runner, false, false)))
    }

    "blocker" >>{
      (for
        g1 <- Gate(1)
        g2 <- Gate(1)
        fib <- IO.blocking { g1.unsafeOpen() ; g2.unsafeEnter() }.fork
        _ <- g1.enter
        s <- fib.status 
        _ <- g2.open
      yield s)
      .warp
      .runIO
      .===(Outcome.Success(Fiber.Status.Pending(Fiber.Role.Blocker, false, false)))
    }

    "waiter" >>{
      (for
        g <- Gate(1)
        fib1 <- g.enter.fork
        fib2 <- fib1.awaitVoid.fork
        _ <- IO.sleep(100)
        s <- fib2.status
        _ <- g.open
        ok = s match
          case Fiber.Status.Pending(Fiber.Role.Waiter(fib1), false, false) => true
          case _ => false
      yield ok)
      .warp
      .runIO
      .===(Outcome.Success(true))
    }

    "racer" >>{
      val r = Fiber.Status.Pending(Fiber.Role.Runner, isRacer = true, isCancelled = false)
      val comp = Fiber.current.flatMap(_.status)
      (comp *! comp)
      .runIO
      .===(Outcome.Success(((r, r))))
    }

    "arbiter" >>{
      (for
        g1 <- Gate(2)
        g2 <- Gate(1)
        comp = g1.open &&! g2.enter
        fib <- (comp *! comp).fork
        _ <- g1.enter
        s <- fib.status
        ok = s match
          case Fiber.Status.Pending(Fiber.Role.Arbiter(List(_, _)), false, false) => true
          case _ => false
        _ <- g2.open
      yield ok)
      .warp
      .runIO
      .===(Outcome.Success(true))
    }

    "cancelled & pending" >>{
      (for
        g1 <- Gate(1)
        g2 <- Gate(1)
        fib <- (g1.open &&! g2.enter).fork
        _ <- g1.enter
        _ <- fib.cancelAndForget
        s <- fib.status 
        _ <- g2.open
      yield s)
      .warp
      .runIO
      .===(Outcome.Success(Fiber.Status.Pending(Fiber.Role.Runner, isRacer = false, isCancelled = true)))
    }

    "cancelled & completed" >>{
      (for
        fib <- IO.sleep(5000).fork
        _ <- fib.cancel
        s <- fib.status 
      yield s)
      .warp
      .runIO
      .===(Outcome.Success(Fiber.Status.Completed(Outcome.Cancelled)))
    }
  }

  //---------------------------------------------------------

  "poll" >> {
    "pending" >>{
      (for
        g <- Gate(1)
        fib <- g.enter.fork
        a <- fib.poll
        b = a.map(_.getIO)
        _ <- g.open
      yield b)
      .warp
      .runIO
      .===(Outcome.Success(None))
    }

    "completed" >>{
      (for
        fib <- !!.pure(42).fork
        _ <- fib.awaitVoid
        a <- fib.poll
        b = a.map(_.get)
      yield b)
      .warp
      .runIO
      .===(Outcome.Success(Some(42)))
    }
  }

  //---------------------------------------------------------

  "join" >> {
    "fork & join pending" >>{
      (for
        g <- Gate(1)
        fib <- (g.open &&! !!.pure(42)).fork
        _ <- g.enter
        a <- fib.join
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(42))
    }

    "(fork & join) x2 pending" >>{
      (for
        g <- Gate(2)
        g1 <- Gate(1)
        g2 <- Gate(1)
        fib1 <- (g.open &&! g1.enter &&! !!.pure(42)).fork
        fib2 <- (g.open &&! g2.enter &&! !!.pure("a")).fork
        _ <- g.enter
        _ <- g1.open
        a <- fib1.join
        _ <- g2.open
        b <- fib2.join
      yield (a, b))
      .warp
      .runIO
      .===(Outcome.Success((42, "a")))
    }

    "fork & (join x2) pending" >>{
      (for
        g <- Gate(1)
        v1 <- AtomicVar(42)
        v2 <- AtomicVar("a")
        fib0 <- (g.enter).fork
        fib1 <- (fib0.join &&! v1.put(1337)).fork
        fib2 <- (fib0.join &&! v2.put("b")).fork
        _ <- g.open
        _ <- fib1.awaitVoid
        _ <- fib2.awaitVoid
        a <- v1.get
        b <- v2.get
      yield (a, b))
      .warp
      .runIO
      .===(Outcome.Success((1337, "b")))
    }

    "fork & join completed" >>{
      (for
        fib <- !!.pure(42).fork
        _ <- IO.sleep(100)
        a <- fib.join
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(42))
    }
  }

  //---------------------------------------------------------

  "cancel" >> {
    "pending" >>{
      (for
        v <- AtomicVar(42)
        fib <- (IO.sleep(100) &&! v.put(1337)).fork
        _ <- fib.cancel
        a <- v.get
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(42))
    }

    "completed" >>{
      (for
        v <- AtomicVar(1337)
        fib <- v.put(42).fork
        _ <- IO.sleep(100)
        _ <- fib.cancel
        a <- v.get
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(42))
    }

    "guarantee" >>{
      (for
        v1 <- AtomicVar(1337)
        v2 <- AtomicVar("a")
        g <- Gate(1)
        fib <- (g.open &&! IO.sleep(100) &&! v2.put("b")).guarantee(v1.put(42)).fork
        _ <- g.enter
        _ <- fib.cancel
        a <- v1.get
        b <- v2.get
      yield (a, b))
      .warp
      .runIO
      .===(Outcome.Success((42, "a")))
    }
  }

  //---------------------------------------------------------

  "nowOrNever" >> {
    "pending" >>{
      (for
        fib <- IO.sleep(100).fork
        zip <- fib.nowOrNever
      yield zip.getIO)
      .warp
      .runIO
      .===(Outcome.Success(Outcome.Cancelled))
    }

    "completed" >>{
      (for
        fib <- !!.pure(42).fork
        _ <- IO.sleep(100)
        zip <- fib.nowOrNever
      yield zip.get)
      .warp
      .runIO
      .===(Outcome.Success(42))
    }
  }

  "getOrDie" >>{
    "pending" >>{
      (for
        fib <- IO.sleep(100).fork
        zip <- fib.getOrDie
      yield zip.getIO)
      .warp
      .runIO
      .isFailure.===(true)
    }

    "completed" >>{
      (for
        fib <- !!.pure(42).fork
        _ <- IO.sleep(100)
        zip <- fib.getOrDie
      yield zip.get)
      .warp
      .runIO
      .===(Outcome.Success(42))
    }
  }

  //---------------------------------------------------------

  "misc" >> {
    "self join" >>{
      (for
        fib <-
          (for
            self <- Fiber.current
            _ <- self.awaitVoid
          yield 42)
          .fork
        _ <- (IO.sleep(100) &&! fib.cancelAndForget).fork
        zip <- fib.await
        a = zip.getIO
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(Outcome.Cancelled))
    }

    "self cancel" >>{
      (for
        fib <-
          (for
            self <- Fiber.current
            _ <- self.cancel
          yield 42)
          .fork
        zip <- fib.await
        a = zip.getIO
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(Outcome.Cancelled))
    }
  }

  //---------------------------------------------------------

  "effectful" >> {
    "using Error" >>{
      case object E extends ErrorK[List, String]
      (for
        fib1 <- E.raise("A").fork
        fib2 <- E.raise("B").fork
        a <- fib1.join *! fib2.join
      yield a)
      .handleWith(E.handlers.all)
      .warp
      .runIO
      .===(Outcome.Success(Left(List("A", "B"))))
    }

    "using Writer" >>{
      case object W extends WriterK[List, String]
      (for
        fib1 <- W.tell("A").fork
        fib2 <- W.tell("B").fork
        _ <- W.tell("1")
        a <- fib1.join *! fib2.join
        _ <- W.tell("2")
      yield a)
      .handleWith(W.handler.justState)
      .warp
      .runIO
      .===(Outcome.Success(List("1", "A", "B", "2")))
    }

    "using Choice" >>{
      case object C extends Choice
      (for
        fib1 <- C("a", "b").fork
        fib2 <- C(1, 2).fork
        a <- C(true, false)
        b <- fib1.join
        c <- C('x', 'y')
        d <- fib2.join
      yield s"$a-$b-$c-$d")
      .handleWith(C.handler)
      .warp
      .runIO
      .===(Outcome.Success(List(
        "true-a-x-1",
        "true-a-x-2",        
        "true-a-y-1",
        "true-a-y-2",        
        "true-b-x-1",
        "true-b-x-2",        
        "true-b-y-1",
        "true-b-y-2",        
        "false-a-x-1",
        "false-a-x-2",        
        "false-a-y-1",
        "false-a-y-2",        
        "false-b-x-1",
        "false-b-x-2",        
        "false-b-y-1",
        "false-b-y-2",        
      )))
    }

    "using local effect & handler" >>{
      case object W1 extends Writer[String]
      case object W2 extends Writer[String]
      (for
        _ <- W1.tell("a")
        fib <- (W1.tell("b") &&! W2.tell("c")).handleWith(W2.handler.justState).fork
        _ <- W1.tell("d")
        x <- fib.join
        _ <- W1.tell("e")
      yield x)
      .handleWith(W1.handler)
      .warp
      .runIO
      .===(Outcome.Success(("c", "adbe")))
    }
  }

  "ZeroThreadedExecutor" >> {
    import turbolift.mode.ST

    "fork" >>{
      (for
        _ <- !!.unit.fork
      yield 42)
      .warpAwait
      .runIO
      .===(Outcome.Success(42))
    }
  }

  "type safety" >> {
    "effectful fiber must inherit its handlers from the environment at fork" >> {
      "wrong" >>{
        Typecheck {"""
          case object W extends Writer[String]
          (for
            fib <- W.tell("a").as(42).fork
            aw <- fib.join.handleWith(W.handler)
          yield aw)
          .warp
          .runIO
          .===(Outcome.Success((42, "a")))
        """} must succeed.not
      }

      "correct" >>{
        case object W extends Writer[String]
        (for
          fib <- W.tell("a").as(42).fork
          a <- fib.join
        yield a)
        .handleWith(W.handler)
        .warp
        .runIO
        .===(Outcome.Success((42, "a")))
      }

      "also correct" >>{
        case object W extends Writer[String]
        (for
          workaround <- W.tell("a").as(42).fork.handleWith(W.handler)
          (fib, w) = workaround
          aw <- fib.join.handleWith(W.handler)
        yield (aw, w))
        .warp
        .runIO
        .===(Outcome.Success(((42, "a"), "")))
      }
    }
  }

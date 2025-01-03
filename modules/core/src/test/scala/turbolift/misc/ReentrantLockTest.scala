package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{AtomicVar, Outcome, Warp, Fiber, ReentrantLock}
import Auxx._


class ReentrantLockTest extends Specification:
  sequential

  "basic" >> {
    "success" >>{
      (for
        lock <- ReentrantLock.create
        a <- lock.use(!!.pure(42))
      yield a)
      .runIO
      .===(Outcome.Success(42))
    }

    "cancel" >>{
      (for
        lock <- ReentrantLock.create
        _ <- lock.use(IO.cancel)
      yield ())
      .runIO
      .===(Outcome.Cancelled)
    }
  }

  "status" >> {
    "unlocked" >>{
      (for
        lock <- ReentrantLock.create
        a <- lock.holdCount
        b <- lock.isHeldByCurrentFiber
        c <- lock.owner
      yield (a, b, c))
      .runIO
      .===(Outcome.Success((0, false, None)))
    }

    "locked" >>{
      (for
        lock <- ReentrantLock.create
        fib <- Fiber.current
        x <- lock.use:
          for
            a <- lock.holdCount
            b <- lock.isHeldByCurrentFiber
            c <- lock.owner
          yield (a, b, c == Some(fib))
      yield x)
      .runIO
      .===(Outcome.Success((1, true, true)))
    }

    "reentry" >>{
      (for
        lock <- ReentrantLock.create
        fib <- Fiber.current
        x <- lock.use:
          lock.use:
            for
              a <- lock.holdCount
              b <- lock.isHeldByCurrentFiber
              c <- lock.owner
            yield (a, b, c == Some(fib))
      yield x)
      .runIO
      .===(Outcome.Success((2, true, true)))
    }
  }

  "with fibers" >> {
    "sequential access ; success" >>{
      (for
        v <- AtomicVar(0)
        g1 <- Gate(1)
        g2 <- Gate(1)
        lock <- ReentrantLock.create
        _ <- Warp.awaiting:
          for
            _ <- (g1.enter &&! v.event(1) &&! lock.use(v.event(2) &&! g2.open &&! v.event(3))).fork
            _ <- (g1.open &&! g2.enter &&! lock.use(v.event(4))).fork
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(1234))
    }

    "sequential access ; cancelled" >>{
      (for
        v <- AtomicVar(0)
        g <- Gate(1)
        lock <- ReentrantLock.create
        _ <- Warp.awaiting:
          for
            _ <- (lock.use(v.event(1) &&! g.open &&! IO.sleep(100) &&! IO.cancel) &&! v.event(2)).fork
            _ <- g.enter &&! (lock.use(v.event(3))).fork
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(13))
    }

    "very sequential access" >>{
      (for
        v <- AtomicVar(0)
        g0 <- Gate(3)
        g1 <- Gate(1)
        g2 <- Gate(1)
        g3 <- Gate(1)
        lock <- ReentrantLock.create
        _ <- Warp.awaiting:
          for
            _ <- g0.open &&!               lock.use(g1.open &&! v.event(1) &&! g3.enter).fork
            _ <- g0.open &&! (g1.enter &&! lock.use(g2.open &&! v.event(2))).fork
            _ <- g0.open &&! (g2.enter &&! lock.use(            v.event(3))).fork
            _ <- g0.enter
            _ <- IO.sleep(100)
            _ <- g3.open
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(123))
    }
  }

  "reentry" >> {
    "basic" >>{
      (for
        v <- AtomicVar(0)
        lock <- ReentrantLock.create
        a <- lock.use(lock.use(v.put(42).as(1337)))
        b <- v.get
      yield (a, b))
      .runIO
      .===(Outcome.Success((1337, 42)))
    }

    "concurrent" >>{
      (for
        v <- AtomicVar(0)
        lock <- ReentrantLock.create
        _ <- Warp.awaiting:
          for
            _ <-
              lock.use:
                for
                  _ <- v.event(1)
                  _ <- IO.sleep(200)
                  _ <- lock.use:
                    for
                      _ <- v.event(2)
                      _ <- IO.sleep(200)
                    yield ()
                  _ <- v.event(3)
                yield ()
              .fork
            _ <- (IO.sleep(100) &&! lock.use(v.event(4))).fork
            _ <- (IO.sleep(300) &&! lock.use(v.event(5))).fork
          yield ()
        a <- v.get
      yield a)
      .runIO
      .===(Outcome.Success(12345))
    }
  }

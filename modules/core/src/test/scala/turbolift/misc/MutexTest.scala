package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.Outcome
import turbolift.io.{AtomicVar, Warp, Fiber, Mutex}
import Auxx._


class MutexTest extends Specification:
  sequential

  "basic" >> {
    "success" >>{
      (for
        lock <- Mutex.create
        a <- lock.use(!!.pure(42))
      yield a)
      .runIO
      .===(Outcome.Success(42))
    }

    "cancel" >>{
      (for
        lock <- Mutex.create
        _ <- lock.use(IO.cancel)
      yield ())
      .runIO
      .===(Outcome.Cancelled)
    }
  }

  "status" >> {
    "unlocked" >>{
      (for
        lock <- Mutex.create
        a <- lock.isLocked
      yield a)
      .runIO
      .===(Outcome.Success(false))
    }

    "locked" >>{
      (for
        lock <- Mutex.create
        fib <- Fiber.current
        x <- lock.use:
          lock.isLocked
      yield x)
      .runIO
      .===(Outcome.Success(true))
    }

  }

  "with fibers" >> {
    "sequential access ; success" >>{
      (for
        v <- AtomicVar(0)
        g1 <- Gate(1)
        g2 <- Gate(1)
        lock <- Mutex.create
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
        lock <- Mutex.create
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
        lock <- Mutex.create
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
        lock <- Mutex.create
        a <- lock.use(lock.tryUse(v.put(42).as(1337)))
        b <- v.get
      yield (a, b))
      .runIO
      .===(Outcome.Success((None, 0)))
    }

    "concurrent" >>{
      (for
        v <- AtomicVar(0)
        lock <- Mutex.create
        _ <- Warp.awaiting:
          for
            _ <-
              lock.use:
                for
                  _ <- v.event(1)
                  _ <- IO.sleep(200)
                  _ <- lock.tryUse:
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
      .===(Outcome.Success(1345))
    }
  }

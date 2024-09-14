package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{AtomicVar, Outcome, Warp, Mutex}
import Auxx._


class MutexTest extends Specification:
  sequential

  "basic" >> {
    "success" >>{
      (for
        mutex <- Mutex.create
        a <- mutex.lock(!!.pure(42))
      yield a)
      .runIO
      .===(Outcome.Success(42))
    }

    "cancel" >>{
      (for
        mutex <- Mutex.create
        _ <- mutex.lock(IO.cancel)
      yield ())
      .runIO
      .===(Outcome.Cancelled)
    }
  }

  "with fibers" >> {
    "sequential access ; success" >>{
      (for
        v <- AtomicVar(0)
        g1 <- Gate(1)
        g2 <- Gate(1)
        mutex <- Mutex.create
        _ <- Warp.awaiting:
          for
            _ <- (g1.enter &&! v.event(1) &&! mutex.lock(v.event(2) &&! g2.open &&! v.event(3))).fork
            _ <- (g1.open &&! g2.enter &&! mutex.lock(v.event(4))).fork
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
        mutex <- Mutex.create
        _ <- Warp.awaiting:
          for
            _ <- (mutex.lock(v.event(1) &&! g.open &&! IO.sleep(100) &&! IO.cancel) &&! v.event(2)).fork
            _ <- g.enter &&! (mutex.lock(v.event(3))).fork
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
        mutex <- Mutex.create
        _ <- Warp.awaiting:
          for
            _ <- g0.open &&!               mutex.lock(g1.open &&! v.event(1) &&! g3.enter).fork
            _ <- g0.open &&! (g1.enter &&! mutex.lock(g2.open &&! v.event(2))).fork
            _ <- g0.open &&! (g2.enter &&! mutex.lock(            v.event(3))).fork
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

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
        mutex <- Mutex.fresh
        a <- mutex.lock(!!.pure(42))
      yield a)
      .runIO
      .===(Outcome.Success(42))
    }

    "cancel" >>{
      (for
        mutex <- Mutex.fresh
        _ <- mutex.lock(IO.cancel)
      yield ())
      .runIO
      .===(Outcome.Cancelled)
    }
  }

  "with fibers" >> {
    "sequential access ; success" >>{
      (for
        v <- AtomicVar.fresh(0)
        g1 <- Gate(1)
        g2 <- Gate(1)
        mutex <- Mutex.fresh
        _ <- Warp.shutdownOnExit:
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
        v <- AtomicVar.fresh(0)
        g <- Gate(1)
        mutex <- Mutex.fresh
        _ <- Warp.shutdownOnExit:
          for
            _ <- (mutex.lock(v.event(1) &&! g.open &&! IO.sleep(100) &&! IO.cancel) &&! v.event(2)).fork
            _ <- g.enter &&! (mutex.lock(v.event(3))).fork
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(13))
    }
  }

package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{AtomicVar, Outcome, Warp, CountDownLatch}
import Auxx._


class CountDownLatchTest extends Specification:
  sequential

  "basic" >> {
    "empty" >>{
      (for
        latch <- CountDownLatch.fresh(0)
        _ <- latch.await
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }

    "one" >>{
      (for
        latch <- CountDownLatch.fresh(1)
        _ <- latch.release
        _ <- latch.await
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }

    "one too many" >>{
      (for
        latch <- CountDownLatch.fresh(1)
        _ <- latch.release
        _ <- latch.release
        _ <- latch.await
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }
  }

  "with fibers" >> {
    "2 await 1 release" >>{
      (for
        v <- AtomicVar.fresh(300)
        g <- Gate(2)
        latch <- CountDownLatch.fresh(1)
        _ <- Warp.shutdownOnExit:
          for
            _ <- (g.open &&! latch.await &&! v.modify(_ + 10)).fork
            _ <- (g.open &&! latch.await &&! v.modify(_ + 02)).fork
            _ <- g.enter
            _ <- v.modify(_ * 10)
            _ <- latch.release
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(3012))
    }

    "1 await 2 release" >>{
      (for
        v <- AtomicVar.fresh(0)
        g <- Gate(1)
        latch <- CountDownLatch.fresh(2)
        _ <- Warp.shutdownOnExit:
          for
            _ <- (g.enter &&! v.modify(_ + 10) &&! latch.release).fork
            _ <- (g.enter &&! v.modify(_ + 02) &&! latch.release).fork
            _ <- (g.open &&! latch.await &&! v.event(3)).fork
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(123))
    }

    "1 cancel 0 release" >>{
      (for
        v <- AtomicVar.fresh(0)
        g <- Gate(1)
        latch <- CountDownLatch.fresh(1)
        _ <- Warp.cancelOnExit:
          for
            _ <- (g.open &&! latch.await &&! v.event(1)).guarantee(v.event(2)).fork
            _ <- g.enter
            _ <- v.event(3)
          yield ()
        _ <- v.event(4)
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(324))
    }
  }

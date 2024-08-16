package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{AtomicVar, Outcome, Warp, CyclicBarrier}
import Auxx._


class CyclicBarrierTest extends Specification:
  sequential

  "basic" >> {
    "empty" >>{
      (for
        barrier <- CyclicBarrier.fresh(0)
        _ <- barrier.await
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }

    "one" >>{
      (for
        barrier <- CyclicBarrier.fresh(1)
        _ <- barrier.await
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }

    "one x2" >>{
      (for
        barrier <- CyclicBarrier.fresh(1)
        _ <- barrier.await
        _ <- barrier.await
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }
  }


  "with fibers" >> {
    "2 rounds" >>{
      (for
        v <- AtomicVar.fresh(0)
        barrier <- CyclicBarrier.fresh(3)
        _ <- Warp.shutdownOnExit:
          for
            _ <- (v.modify(_ + 100) &&! barrier.await &&! v.modify(_ * 10) &&! barrier.await &&! v.modify(_ + 400)).fork
            _ <- (v.modify(_ + 020) &&! barrier.await &&! v.modify(_ * 10) &&! barrier.await &&! v.modify(_ + 050)).fork
            _ <- (v.modify(_ + 003) &&! barrier.await &&! v.modify(_ * 10) &&! barrier.await &&! v.modify(_ + 006)).fork
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(123456))
    }


    "not enough participants" >>{
      (for
        v <- AtomicVar.fresh(0)
        barrier <- CyclicBarrier.fresh(3)
        _ <- Warp.cancelOnExit:
          for
            _ <- (v.modify(_ + 10) &&! barrier.await &&! v.modify(_ + 3000)).fork
            _ <- (v.modify(_ + 02) &&! barrier.await &&! v.modify(_ + 0400)).fork
            _ <- IO.sleep(100)
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(12))
    }
  }

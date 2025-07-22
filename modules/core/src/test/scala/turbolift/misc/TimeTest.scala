package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.data.Outcome
import turbolift.io.{Warp, AtomicVar}
import turbolift.effects.IO
import turbolift.misc.Auxx._


class TimeTest extends Specification:
  sequential

  "Basic ops" >> {
    "sleep" >>{
      IO.sleep(1).runIO === Outcome.Success(())
    }

    "fork & sleep & join" >>{
      (for
        fib <- (IO.sleep(10) &&! !!.pure(42)).fork
        a <- fib.join
      yield a)
      .warp
      .runIO === Outcome.Success(42)
    }

    "fork & sleep & cancel" >>{
      (for
        v <- AtomicVar(42)
        fib <- (IO.sleep(200) &&! v.put(1337)).fork
        _ <- fib.cancel
        a <- v.get
      yield a)
      .warp
      .runIO === Outcome.Success(42)
    }

    "delay" >>{
      (for
        v <- AtomicVar(0)
        _ <- Warp.awaiting:
          for
            _ <- v.event(3).delay(30).fork
            _ <- v.event(2).delay(20).fork
            _ <- v.event(1).delay(10).fork
          yield ()
        x <- v.get
      yield x)
      .runIO === Outcome.Success(123)
    }
  }

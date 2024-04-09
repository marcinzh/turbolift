package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.io.{Warp, Outcome, AtomicVar}
import turbolift.effects.IO


class TimeTest extends Specification:
  "Basic ops" >> {
    "sleep" >>{
      IO.sleep(1).unsafeRun === Outcome.Success(())
    }

    "fork & sleep & join" >>{
      (for
        fib <- (IO.sleep(10) &&! !!.pure(42)).fork
        a <- fib.join
      yield a)
      .unsafeRun === Outcome.Success(42)
    }

    "fork & sleep & cancel" >>{
      (for
        v <- AtomicVar.fresh(42)
        fib <- (IO.sleep(200) &&! v.put(1337)).fork
        _ <- fib.cancel
        a <- v.get
      yield a)
      .unsafeRun === Outcome.Success(42)
    }
  }

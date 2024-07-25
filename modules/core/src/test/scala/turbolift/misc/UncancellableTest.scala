package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, Error}
import turbolift.effects.CanLaunchTheMissiles
import turbolift.io.{Outcome, Cause, AtomicVar, Fiber}
import Auxx._


class UncancellableTest extends Specification:
  sequential

  "basic" >> {
    "uncancellable" >>{
      IO.uncancellable(!!.pure(42)).runIO.===(Outcome.Success(42))
    }

    "cancellable" >>{
      IO.cancellable(!!.pure(42)).runIO.===(Outcome.Success(42))
    }
  }

  "combined" >> {
    "cancel uncancellable" >>{
      (for
        v <- AtomicVar.fresh(1)
        g <- Gate(1)
        fib <- Fiber.fork:
          IO.uncancellable:
            g.open &&! IO.sleep(100) &&! v.event(2)
        _ <- g.enter
        _ <- fib.cancel
        a <- v.get
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(12))
    }

    "cancel uncancellable(cancellable(_))" >>{
      (for
        v <- AtomicVar.fresh(1)
        g <- Gate(1)
        fib <- Fiber.fork:
          IO.uncancellable:
            IO.cancellable:
              g.open &&! IO.sleep(100) &&! v.event(2)
        _ <- g.enter
        _ <- fib.cancel
        a <- v.get
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(1))
    }

    "cancel after uncancellable" >>{
      (for
        v <- AtomicVar.fresh(1)
        g <- Gate(1)
        fib <- Fiber.fork:
          IO.uncancellable(!!.unit) &&! g.open &&! IO.sleep(100) &&! v.event(2)
        _ <- g.enter
        _ <- fib.cancel
        a <- v.get
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(1))
    }

    "cancel after cancellable" >>{
      (for
        v <- AtomicVar.fresh(1)
        g <- Gate(1)
        fib <- Fiber.fork:
          IO.uncancellable:
            IO.cancellable(!!.unit) &&! g.open &&! IO.sleep(100) &&! v.event(2)
        _ <- g.enter
        _ <- fib.cancel
        a <- v.get
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(12))
    }
  }

  "self-cancel in uncancellable" >> {
    "IO.cancel" >>{
      IO.uncancellable(IO.cancel).runIO.===(Outcome.Cancelled)
    }

    "Fiber.cancel" >>{
      IO.uncancellable:
        Fiber.current.flatMap(_.cancel)
      .runIO.===(Outcome.Cancelled)
    }
  }

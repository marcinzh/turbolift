package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects._
import turbolift.io.{Outcome, Snap, Cause, AtomicVar, ResourceFactory}
import turbolift.effects.{Finalizer, FinalizerEffect}
import Auxx._


class ResourceTest extends Specification:
  sequential

  def basicRF(v: AtomicVar[Int], acq: Int, rel: Int) = ResourceFactory[Unit, IO](v.event(acq), _ => v.event(rel))

  "basic" >> {
    "one res" >>{
      (for
        v <- AtomicVar(1)
        rf = basicRF(v, 2, 3)
        _ <-
          (for
            _ <- v.event(4)
            _ <- Finalizer.use(rf)
            _ <- v.event(5)
          yield ())
          .finalized
        _ <- v.event(6)
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(142536))
    }

    "two res" >>{
      (for
        v <- AtomicVar(1)
        rf1 = basicRF(v, 2, 3)
        rf2 = basicRF(v, 4, 5)
        _ <-
          (for
            _ <- v.event(6)
            _ <- Finalizer.use(rf1)
            _ <- v.event(7)
            _ <- Finalizer.use(rf2)
            _ <- v.event(8)
          yield ())
          .finalized
        _ <- v.event(9)
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(162748539))
    }
  }

  "with exceptions" >> {
    val ex1 = new Exception("EX1")
    val ex2 = new Exception("EX2")

    "in acquire" >>{
      (for
        v <- AtomicVar(1)
        e <- IO.catchToEither:
          (for
            _ <- Finalizer.use(IO(throw ex1), _ => v.event(2))
            _ <- v.event(3)
          yield ())
          .finalized
        n <- v.get
      yield (n, e))
      .runIO
      .===(Outcome.Success((1, Left(ex1))))
    }

    "in release" >>{
      (for
        v <- AtomicVar(1)
        e <- IO.catchToEither:
          (for
            _ <- Finalizer.use(v.event(2), _ => IO(throw ex1))
            _ <- v.event(3)
          yield ())
          .finalized
        _ <- v.event(4)
        n <- v.get
      yield (n, e))
      .runIO
      .===(Outcome.Success((1234, Left(ex1))))
    }

    "in release x2" >>{
      (for
        v <- AtomicVar(1)
        e <- IO.snap:
          (for
            _ <- Finalizer.use(v.event(2), _ => IO(throw ex1))
            _ <- v.event(3)
            _ <- Finalizer.use(v.event(4), _ => IO(throw ex2))
            _ <- v.event(5)
          yield ())
          .finalized
        _ <- v.event(6)
        n <- v.get
      yield (n, e))
      .runIO
      .===(Outcome.Success((123456, Snap.Failure(Cause.Then(Cause.Thrown(ex2), Cause.Thrown(ex1))))))
    }
  }

  "parallel" >> {
    def parRF(v: AtomicVar[Long], g1: Gate, g2: Gate, acq: Int, rel: Int) =
      def stuff(g: Gate, n: Int) = v.event(n) &&! g.enter &&! v.event(n + 1)
      ResourceFactory[Unit, IO](stuff(g1, acq), _ => stuff(g2, rel))

    def seqRF(v: AtomicVar[Long], acq: Int, rel: Int) =
      ResourceFactory[Unit, IO](v.event(acq), _ => v.event(rel))

    def mkClock(v: AtomicVar[Long], g1: Gate, g2: Gate) =
      IO.sleep(100) &&! g1.open &&!
      IO.sleep(100) &&! g2.open

    "x2 par" >>{
      (for
        v <- AtomicVar(0L)
        g1 <- Gate(1)
        g2 <- Gate(1)
        rf = parRF(v, g1, g2, 1, 3)
        clock = mkClock(v, g1, g2)
        _ <-
          (for
            _ <- clock.fork
            _ <- Finalizer.use(rf) *! Finalizer.use(rf)
            _ <- v.event(0)
          yield ())
          .finalized
        n <- v.get
      yield n)
      .warp
      .runIO
      .===(Outcome.Success(112203344L))
    }

    "par + seq" >>{
      (for
        v <- AtomicVar(0L)
        g1 <- Gate(1)
        g2 <- Gate(1)
        rf1 = seqRF(v, 1, 2)
        rf2 = parRF(v, g1, g2, 3, 5)
        rf3 = seqRF(v, 7, 8)
        clock = mkClock(v, g1, g2)
        _ <-
          (for
            _ <- Finalizer.use(rf1)
            _ <- clock.fork
            _ <- Finalizer.use(rf2) *! Finalizer.use(rf2)
            _ <- Finalizer.use(rf3)
            _ <- v.event(0)
          yield ())
          .finalized
        n <- v.get
      yield n)
      .warp
      .runIO
      .===(Outcome.Success(1334470855662L))
    }
  }

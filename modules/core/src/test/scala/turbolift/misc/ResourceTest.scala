package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{Finalizer, IO}
import turbolift.data.{Outcome, Snap, Cause, Resource}
import turbolift.io.{AtomicVar}
import Auxx._


class ResourceTest extends Specification:
  sequential

  def basicRes(v: AtomicVar[Int], acq: Int, rel: Int) = Resource(v.event(acq), v.event(rel))

  "basic" >> {
    "one res" >>{
      (for
        v <- AtomicVar(1)
        res = basicRes(v, 2, 3)
        _ <-
          (for
            _ <- v.event(4)
            _ <- Finalizer.use(res)
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
        res1 = basicRes(v, 2, 3)
        res2 = basicRes(v, 4, 5)
        _ <-
          (for
            _ <- v.event(6)
            _ <- Finalizer.use(res1)
            _ <- v.event(7)
            _ <- Finalizer.use(res2)
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

    "in use" >>{
      (for
        v <- AtomicVar(1)
        e <- IO.catchToEither:
          (for
            _ <- Finalizer.use(v.event(2), v.event(3))
            _ <- v.event(4)
            _ <- IO(throw ex1)
            _ <- v.event(5)
          yield ())
          .finalized
        _ <- v.event(6)
        n <- v.get
      yield (n, e))
      .runIO
      .===(Outcome.Success((12436, Left(ex1))))
    }

    "in acquire" >>{
      (for
        v <- AtomicVar(1)
        e <- IO.catchToEither:
          (for
            _ <- Finalizer.use(IO(throw ex1), v.event(2))
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
            _ <- Finalizer.use(v.event(2), IO(throw ex1))
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
            _ <- Finalizer.use(v.event(2), IO(throw ex1))
            _ <- v.event(3)
            _ <- Finalizer.use(v.event(4), IO(throw ex2))
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

  "nested" >>{
    (for
      v <- AtomicVar(0L)
      _ <- 
        (for
          _ <- Finalizer.use(v.event(1), v.event(2))
          _ <- v.event(3)
          _ <- 
            (for
              _ <- Finalizer.use(v.event(4), v.event(5))
              _ <- v.event(6)
            yield ())
            .finalized
          _ <- v.event(7)
        yield ())
        .finalized
      n <- v.get
    yield n)
    .runIO
    .===(Outcome.Success(1346572))
  }

  "parallel" >> {
    def parRes(v: AtomicVar[Long], g1: Gate, g2: Gate, acq: Int, rel: Int) =
      def stuff(g: Gate, n: Int) = v.event(n) &&! g.enter &&! v.event(n + 1)
      Resource[Unit, IO](stuff(g1, acq), _ => stuff(g2, rel))

    def seqRes(v: AtomicVar[Long], acq: Int, rel: Int) =
      Resource[Unit, IO](v.event(acq), _ => v.event(rel))

    def mkClock(v: AtomicVar[Long], g1: Gate, g2: Gate) =
      IO.sleep(100) &&! g1.open &&!
      IO.sleep(100) &&! g2.open

    "x2 par" >>{
      (for
        v <- AtomicVar(0L)
        g1 <- Gate(1)
        g2 <- Gate(1)
        res = parRes(v, g1, g2, 1, 3)
        clock = mkClock(v, g1, g2)
        _ <-
          (for
            _ <- clock.fork
            _ <- Finalizer.use(res) *! Finalizer.use(res)
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
        res1 = seqRes(v, 1, 2)
        res2 = parRes(v, g1, g2, 3, 5)
        res3 = seqRes(v, 7, 8)
        clock = mkClock(v, g1, g2)
        _ <-
          (for
            _ <- Finalizer.use(res1)
            _ <- clock.fork
            _ <- Finalizer.use(res2) *! Finalizer.use(res2)
            _ <- Finalizer.use(res3)
            _ <- v.event(0)
          yield ())
          .finalized
        n <- v.get
      yield n)
      .warp
      .runIO
      .===(Outcome.Success(1334470855662L))
    }

    "with exceptions" >>{
      val ex1 = new Exception("EX1")

      (for
        v <- AtomicVar(0L)
        g1 <- Gate(1)
        g2 <- Gate(1)
        rf = parRes(v, g1, g2, 1, 3)
        clock = mkClock(v, g1, g2)
        e <- IO.catchToEither:
          (for
            _ <- clock.fork
            _ <- Finalizer.use(rf) *! Finalizer.use(rf)
            _ <- v.event(0)
            _ <- IO(throw ex1)
          yield ())
          .finalized
        n <- v.get
      yield (n, e))
      .warp
      .runIO
      .===(Outcome.Success((112203344L, Left(ex1))))
    }
  }

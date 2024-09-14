package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{AtomicVar, Outcome, Warp, Semaphore}
import Auxx._


class SemaphoreTest extends Specification:
  sequential

  "basic" >> {
    "acquire" >>{
      (for
        semaphore <- Semaphore(1)
        _ <- semaphore.acquire(1)
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }

    "release" >>{
      (for
        semaphore <- Semaphore(0)
        _ <- semaphore.release(1)
        _ <- semaphore.acquire(1)
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }

    "use" >>{
      (for
        semaphore <- Semaphore(1)
        a <- semaphore.use(1)(!!.pure(42))
      yield a)
      .runIO
      .===(Outcome.Success(42))
    }
  }


  "with fibers" >> {
    "waiting" >>{
      (for
        v <- AtomicVar(0)
        semaphore <- Semaphore(1)
        _ <- Warp.awaiting:
          for
            _ <- semaphore.use(1)(IO.sleep(200) &&! v.event(1)).fork
            _ <- (IO.sleep(100) &&! semaphore.use(1)(v.event(2))).fork
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(12))
    }

    "order" >>{
      (for
        v <- AtomicVar(0)
        semaphore <- Semaphore(0)
        _ <- Warp.awaiting:
          for
            _ <- (semaphore.acquire(1) &&! v.modify(_ + 10)).fork
            _ <- (semaphore.acquire(1) &&! v.modify(_ + 02)).fork
            _ <- IO.sleep(10)
            _ <- (semaphore.acquire(1) &&! v.modify(_ + 30)).fork
            _ <- (semaphore.acquire(1) &&! v.modify(_ + 04)).fork
            _ <- IO.sleep(10)
            _ <- semaphore.release(2)
            _ <- IO.sleep(10)
            _ <- v.modify(_ * 1000)
            _ <- semaphore.release(2)
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(12034))
    }

    "partial releases" >>{
      (for
        v <- AtomicVar(0)
        semaphore <- Semaphore(0)
        _ <- Warp.awaiting:
          for
            _ <- (semaphore.acquire(10) &&! v.event(1)).fork
            _ <- IO.sleep(10)
            _ <- (semaphore.acquire(10) &&! v.event(2)).fork
            _ <- IO.sleep(10)
            _ <- semaphore.release(5)
            _ <- IO.sleep(10) &&! v.event(3)
            _ <- semaphore.release(10)
            _ <- IO.sleep(10) &&! v.event(4)
            _ <- semaphore.release(10)
            _ <- IO.sleep(10) &&! v.event(5)
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(31425))
    }

    "cancel first waiter" >>{
      (for
        v <- AtomicVar(0)
        semaphore <- Semaphore(5)
        _ <- Warp.awaiting:
          for
            fib1 <- (semaphore.acquire(10) &&! v.event(1)).fork
            _ <- IO.sleep(10)
            fib2 <- (semaphore.acquire(5) &&! v.event(2)).fork
            _ <- IO.sleep(10) &&! v.event(3)
            _ <- IO.sleep(10) &&! fib1.cancel
            _ <- IO.sleep(10) &&! v.event(4)
          yield ()
        n <- v.get
      yield n)
      .runIO
      .===(Outcome.Success(324))
    }
  }

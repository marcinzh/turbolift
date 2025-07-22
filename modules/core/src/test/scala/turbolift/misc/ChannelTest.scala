package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.Outcome
import turbolift.io.{AtomicVar, Channel, Warp}
import Auxx._


class ChannelTest extends Specification:
  sequential

  "basic" >> {
    "unbounded" >> {
      "put get" >>{
        (for
          channel <- Channel.unbounded[Int]
          _ <- channel.put(42)
          a <- channel.get
        yield a)
        .runIO
        .===(Outcome.Success(42))
      }

      "put put get get" >>{
        (for
          channel <- Channel.unbounded[Int]
          _ <- channel.put(1)
          _ <- channel.put(2)
          a <- channel.get
          b <- channel.get
        yield (a, b))
        .runIO
        .===(Outcome.Success((1, 2)))
      }

      "put get put get" >>{
        (for
          channel <- Channel.unbounded[Int]
          _ <- channel.put(1)
          a <- channel.get
          _ <- channel.put(2)
          b <- channel.get
        yield (a, b))
        .runIO
        .===(Outcome.Success((1, 2)))
      }

      "tryPut tryGet" >>{
        (for
          channel <- Channel.unbounded[Int]
          a <- channel.tryPut(42)
          b <- channel.tryGet
        yield (a, b))
        .runIO
        .===(Outcome.Success((true, Some(42))))
      }

      "tryGet tryPut" >>{
        (for
          channel <- Channel.unbounded[Int]
          a <- channel.tryGet
          b <- channel.tryPut(42)
        yield (a, b))
        .runIO
        .===(Outcome.Success((None, true)))
      }
    }

    "bounded" >> {
      "put get" >>{
        (for
          channel <- Channel.bounded[Int](1)
          _ <- channel.put(42)
          a <- channel.get
        yield a)
        .runIO
        .===(Outcome.Success(42))
      }

      "put put get get" >>{
        (for
          channel <- Channel.bounded[Int](2)
          _ <- channel.put(1)
          _ <- channel.put(2)
          a <- channel.get
          b <- channel.get
        yield (a, b))
        .runIO
        .===(Outcome.Success((1, 2)))
      }

      "put get put get" >>{
        (for
          channel <- Channel.bounded[Int](2)
          _ <- channel.put(1)
          a <- channel.get
          _ <- channel.put(2)
          b <- channel.get
        yield (a, b))
        .runIO
        .===(Outcome.Success((1, 2)))
      }
    }

    "synchronous" >> {
      "put &! get" >>{
        (for
          channel <- Channel.synchronous[Int]
          a <- channel.put(42) &! (IO.sleep(10) &&! channel.get)
        yield a)
        .runIO
        .===(Outcome.Success(42))
      }

      "get &! put" >>{
        (for
          channel <- Channel.synchronous[Int]
          a <- channel.get &<! (IO.sleep(10) &&! channel.put(42))
        yield a)
        .runIO
        .===(Outcome.Success(42))
      }

      "tryPut tryGet" >>{
        (for
          channel <- Channel.synchronous[Int]
          a <- channel.tryPut(42)
          b <- channel.tryGet
        yield (a, b))
        .runIO
        .===(Outcome.Success((false, None)))
      }
    }
  }


  "with fibers" >> {
    "bounded" >> {
      "block on put" >>{
        (for
          v <- AtomicVar(0)
          channel <- Channel.bounded[Int](1)
          _ <- channel.put(1)
          fib <- (channel.put(2) &&! v.event(1)).fork
          _ <- IO.sleep(10) &&! v.event(2)
          a <- channel.get
          _ <- IO.sleep(10) &&! v.event(3)
          b <- channel.get
          n <- v.get
        yield (a, b, n))
        .warpAwait
        .runIO
        .===(Outcome.Success((1, 2, 213)))
      }

      "block on get" >>{
        (for
          v <- AtomicVar(0)
          channel <- Channel.bounded[Int](1)
          fib1 <- (channel.get &&<! v.event(1)).fork
          _ <- IO.sleep(10) &&! v.event(2)
          fib2 <- (channel.get &&<! v.event(3)).fork
          _ <- IO.sleep(10) &&! v.event(4)
          _ <- channel.put(1)
          _ <- IO.sleep(10) &&! v.event(5)
          _ <- channel.put(2)
          a <- fib1.join
          b <- fib2.join
          n <- v.get
        yield (a, b, n))
        .warpAwait
        .runIO
        .===(Outcome.Success((1, 2, 24153)))
      }
    }

    "synchronous" >> {
      "block on put" >>{
        (for
          v <- AtomicVar(0)
          channel <- Channel.synchronous[Int]
          _ <- (channel.put(1) &&! v.event(1)).fork
          _ <- IO.sleep(10) &&! v.event(2)
          _ <- (channel.put(2) &&! v.event(3)).fork
          _ <- IO.sleep(10) &&! v.event(4)
          a <- channel.get &&<! IO.sleep(10)
          b <- channel.get &&<! IO.sleep(10)
          n <- v.get
        yield (a, b, n))
        .warpAwait
        .runIO
        .===(Outcome.Success((1, 2, 2413)))
      }

      "block on get" >>{
        (for
          v <- AtomicVar(0)
          channel <- Channel.synchronous[Int]
          fib1 <- (channel.get &&<! v.event(1)).fork
          _ <- IO.sleep(10) &&! v.event(2)
          fib2 <- (channel.get &&<! v.event(3)).fork
          _ <- IO.sleep(10) &&! v.event(4)
          a <- channel.put(1) &&<! IO.sleep(10)
          b <- channel.put(2) &&<! IO.sleep(10)
          a <- fib1.join
          b <- fib2.join
          _ <- IO.sleep(10)
          n <- v.get
        yield (a, b, n))
        .warpAwait
        .runIO
        .===(Outcome.Success((1, 2, 2413)))
      }
    }
  }

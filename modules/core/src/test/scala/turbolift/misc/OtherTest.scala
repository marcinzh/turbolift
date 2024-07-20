package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects._
import turbolift.io.{Outcome}


class OtherTest extends Specification:
  sequential

  //// Regression in OpCascaded.fork1/fork2 rewrite #50163b397
  "OpCascaded.fork" >> {
    case object W extends WriterK[List, Int]
    case object Dummy1 extends Reader[Boolean]
    case object Dummy2 extends Reader[Double]

    val handler =
      Dummy1.handler(true) &&&!
      W.handler.justState &&&!
      Dummy2.handler(0.0)

    val result = Outcome.Success(List(1, 2, 3, 4))

    "fork1" >>{
      (for
        _ <- W.tell(1)
        fib1 <- W.tell(2).fork
        fib2 <- W.tell(3).fork
        _ <- fib1.join
        _ <- fib2.join
        _ <- W.tell(4)
      yield ())
      .warp
      .handleWith(handler)
      .runIO
      .===(result)
    }

    "fork2" >>{
      (for
        _ <- W.tell(1)
        _ <- W.tell(2) *! W.tell(3)
        _ <- W.tell(4)
      yield ())
      .handleWith(handler)
      .runIO
      .===(result)
    }
  }

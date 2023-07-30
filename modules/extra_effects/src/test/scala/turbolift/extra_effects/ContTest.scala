package turbolift.extra_effects
import org.specs2.mutable._
import org.specs2.execute.Result
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.Writer
import turbolift.extra_effects.Cont
import turbolift.mode.ST


class ContTest extends Specification:
  // (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))) = 117
  "shift & reset" >> {
    case object C extends Cont[Int]
    type C = C.type

    C.reset:
      C.shift[Int, C]: k =>
        k(3)
        .flatMap(k)
        .map(_ + 100)
      .map(_ + 2)
    .map(_ + 10)
    .handleWith(C.handler)
    .run === 117
  }

  "shift without reset" >> {
    case object C extends Cont[Int]
    type C = C.type

    /*C.reset*/ {
      C.shift[Int, C]: k =>
        k(3)
        .flatMap(k)
        .map(_ + 100)
      .map(_ + 2)
    }
    .handleWith(C.handler)
    .run === 107
  }

  "cont & writer" >> {
    case object C extends Cont[Unit]
    case object W extends Writer[String]
    type C = C.type
    type W = W.type

    (for
      _ <- C.reset[C & W]:
        for
          _ <- W.tell("1")
          _ <- C.shift[Unit, C & W]: k =>
            for
              _ <- W.tell("2")
              _ <- k(())
              _ <- W.tell("3")
            yield ()
          _ <- W.tell("4")
        yield ()
      _ <- W.tell("5")
    yield ())
    .handleWith(C.handler)
    .handleWith(W.handler.justState)
    .run === "12435"
  }

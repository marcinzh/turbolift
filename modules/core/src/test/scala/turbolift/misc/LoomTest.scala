package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, Error, Writer}
import turbolift.io.{Fiber, Warp, Loom, Outcome, AtomicVar, OnceVar}
import Auxx._


class LoomTest extends Specification:
  sequential

  "basic" >> {
    (for
      loom <- Loom.create[Int, IO]
      _ <- loom.submit(!!.pure(1).delay(10))
      _ <- loom.submit(!!.pure(2).delay(20))
      _ <- loom.done.delay(30).fork
      x <- loom.fold(Nil: List[Int])(_ :+ _)
    yield x)
    .warp
    .runIO
    .===(Outcome.Success(List(1, 2)))
  }


  "effectful" >> {
    case object E extends Error[String]; type E = E.type
    case object W extends Writer[String]; type W = W.type

    "writer" >> {
      (for
        loom <- Loom.create[Unit, W & IO]
        _ <- loom.submit(W.tell("a").delay(10))
        _ <- loom.submit(W.tell("b").delay(20))
        _ <- loom.submit(W.tell("c").delay(30))
        _ <- loom.done.delay(40).fork
        _ <- loom.reduceVoid
      yield ())
      .warp
      .handleWith(W.handler)
      .runIO
      .===(Outcome.Success(((), "abc")))
    }

    "writer & error" >> {
      (for
        loom <- Loom.create[Unit, E & W & IO]
        _ <- loom.submit(W.tell("a").delay(10))
        _ <- loom.submit(W.tell("b").delay(20))
        _ <- loom.submit(E.raise("OMG").delay(30))
        _ <- loom.submit(W.tell("c").delay(40))
        _ <- loom.done.delay(50).fork
        _ <- loom.reduceVoid
      yield ())
      .warp
      .handleWith(E.handler)
      .handleWith(W.handler)
      .runIO
      .===(Outcome.Success((Left("OMG"), "ab")))
    }

    //@#@TODO custom fold with early exit on error
  }
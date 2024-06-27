package turbolift.effects
import org.specs2.mutable._
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.Writer
import turbolift.effects.Cont
import turbolift.mode.ST


class ContTest extends Specification:
  "Basic ops" >> {
    "just reset" >>{
      case object K extends Cont[Int]
      type K = K.type

      !!.pure(1).flatMap: _ =>
        K.reset:
          !!.pure(2)
      .handleWith(K.handler)
      .run === 2
    }

    "shift with cont unapplied" >>{
      case object K extends Cont[Int]
      type K = K.type

      K.shift[Int, K]: k =>
        !!.pure(1)
      .map(_ * 10)
      .handleWith(K.handler)
      .run === 1
    }

    "shift with cont applied once" >>{
      case object K extends Cont[Int]
      type K = K.type

      K.shift[Int, K]: k =>
        k(1)
      .map(_ * 10)
      .handleWith(K.handler)
      .run === 10
    }


    "shift with cont applied twice" >>{
      case object K extends Cont[Int]
      type K = K.type

      K.shift[Int, K]: k =>
        for
          a <- k(1)
          b <- k(200)
        yield a + b
      .map(_ * 10)
      .handleWith(K.handler)
      .run === 2010
    }
  }


  "Complex ops" >> {
    // (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))) = 117
    "shift & reset" >>{
      case object K extends Cont[Int]
      type K = K.type

      K.reset:
        K.shift[Int, K]: k =>
          k(3)
          .flatMap(k)
          .map(_ + 100)
        .map(_ + 2)
      .map(_ + 10)
      .handleWith(K.handler)
      .run === 117
    }

    "shift without reset" >>{
      case object K extends Cont[Int]
      type K = K.type

      /*K.reset*/ {
        K.shift[Int, K]: k =>
          k(3)
          .flatMap(k)
          .map(_ + 100)
        .map(_ + 2)
      }
      .handleWith(K.handler)
      .run === 107
    }

    "cont & writer" >>{
      case object K extends Cont[Unit]
      case object W extends Writer[String]
      type K = K.type
      type W = W.type

      (for
        _ <- K.reset[K & W]:
          for
            _ <- W.tell("1")
            _ <- K.shift[Unit, K & W]: k =>
              for
                _ <- W.tell("2")
                _ <- k(())
                _ <- W.tell("3")
              yield ()
            _ <- W.tell("4")
          yield ()
        _ <- W.tell("5")
      yield ())
      .handleWith(K.handler)
      .handleWith(W.handler.justState)
      .run === "12435"
    }
  }

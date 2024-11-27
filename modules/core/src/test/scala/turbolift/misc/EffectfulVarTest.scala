package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects._
import turbolift.io.{Outcome, Snap, EffectfulVar, Zipper, Warp}


class EffectfulVarTest extends Specification:
  sequential

  def makeZipper[A](a: A): Zipper[A, Any] !! IO = !!.pure(a).fork.flatMap(_.await).warp

  "Basic ops" >> {
    "put" >>{
      (for
        evar <- EffectfulVar[Int, IO]
        zipp <- makeZipper(42)
        _ <- evar.put(zipp)
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }

    "tryPut" >>{
      (for
        evar <- EffectfulVar[Int, IO]
        zipp <- makeZipper(42)
        a <- evar.tryPut(zipp)
      yield a)
      .runIO
      .===(Outcome.Success(true))
    }
  }


  "Combined ops" >> {
    "put & get" >>{
      (for
        evar <- EffectfulVar[Int, IO]
        zipp <- makeZipper(42)
        _ <- evar.put(zipp)
        a <- evar.get
      yield a)
      .runIO
      .===(Outcome.Success(42))
    }

    "put & tryPut" >>{
      (for
        evar <- EffectfulVar[Int, IO]
        zipp1 <- makeZipper(42)
        zipp2 <- makeZipper(1337)
        _ <- evar.put(zipp1)
        a <- evar.tryPut(zipp2)
      yield a)
      .runIO
      .===(Outcome.Success(false))
    }

    "put & put & get" >>{
      (for
        evar <- EffectfulVar[Int, IO]
        zipp1 <- makeZipper(42)
        zipp2 <- makeZipper(1337)
        _ <- evar.put(zipp1)
        _ <- evar.put(zipp2)
        a <- evar.get
      yield a)
      .runIO
      .===(Outcome.Success(42))
    }

    "get & fork(put)" >>{
      (for
        evar <- EffectfulVar[Int, IO]
        zipp <- makeZipper(42)
        _ <- (IO.sleep(100) &&! evar.put(zipp)).fork
        a <- evar.get
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(42))
    }

    "fork & get & cancel" >>{
      (for
        evar <- EffectfulVar[Int, IO]
        fib <- evar.get.fork
        _ <- fib.cancel
      yield ())
      .warp
      .runIO
      .===(Outcome.Success(()))
    }
  }


  "Effectful ops" >> {
    case object W extends Writer[String]; type W = W.type
    case object E extends Error[String]; type E = E.type

    "with Writer" >>{
      (for
        evar <- EffectfulVar[Int, W & IO]
        fib1 <- evar.get.fork
        fib2 <- evar.get.fork
        _ <- IO.sleep(100)
        zipp <- W.tell("a").as(42).fork.flatMap(_.await)
        _ <- evar.put(zipp)
        a <- fib1.join
        b <- fib2.join
      yield (a, b))
      .handleWith(W.handler)
      .warp
      .runIO
      .===(Outcome.Success(((42, 42), "a")))
    }

    "memoize with Writer" >>{
      (for
        evar <- EffectfulVar.memoize(IO.sleep(100) &&! W.tell("a").as(42))
        ab <- evar.get *! evar.get
      yield ab)
      .handleWith(W.handler)
      .warp
      .runIO
      .===(Outcome.Success(((42, 42), "a")))
    }

    "memoize with Error" >>{
      (for
        evar <- EffectfulVar.memoize(IO.sleep(100) &&! E.raise("a").as(42))
        fib1 <- evar.get.fork
        fib2 <- evar.get.fork
        a <- E.toEither(fib1.join)
        b <- E.toEither(IO.snap(fib2.join))
      yield (a, b))
      .handleWith(E.handlers.all)
      .map(_.getOrElse(???))
      .warp
      .runIO
      .===(Outcome.Success((Left("a"), Right(Snap.Cancelled))))
    }
  }
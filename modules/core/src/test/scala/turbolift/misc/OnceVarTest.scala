package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.Outcome
import turbolift.io.{OnceVar, Warp}


class OnceVarTest extends Specification:
  sequential

  "Basic ops" >> {
    "tryGet" >>{
      (for
        ovar <- OnceVar[Int]
        a <- ovar.tryGet
      yield a)
      .runIO
      .===(Outcome.Success(None))
    }

    "put" >>{
      (for
        ovar <- OnceVar[Int]
        _ <- ovar.put(42)
      yield ())
      .runIO
      .===(Outcome.Success(()))
    }

    "tryPut" >>{
      (for
        ovar <- OnceVar[Int]
        a <- ovar.tryPut(42)
      yield a)
      .runIO
      .===(Outcome.Success(true))
    }
  }


  "Combined ops" >> {
    "put & get" >>{
      (for
        ovar <- OnceVar[Int]
        _ <- ovar.put(42)
        a <- ovar.get
      yield a)
      .runIO
      .===(Outcome.Success(42))
    }

    "put & tryGet" >>{
      (for
        ovar <- OnceVar[Int]
        _ <- ovar.put(42)
        a <- ovar.tryGet
      yield a)
      .runIO
      .===(Outcome.Success(Some(42)))
    }

    "put & tryPut" >>{
      (for
        ovar <- OnceVar[Int]
        _ <- ovar.put(42)
        a <- ovar.tryPut(1337)
      yield a)
      .runIO
      .===(Outcome.Success(false))
    }

    "put & put & get" >>{
      (for
        ovar <- OnceVar[Int]
        _ <- ovar.put(42)
        _ <- ovar.put(1337)
        a <- ovar.get
      yield a)
      .runIO
      .===(Outcome.Success(42))
    }

    "get & fork(put)" >>{
      (for
        ovar <- OnceVar[Int]
        _ <- (IO.sleep(100) &&! ovar.put(42)).fork
        a <- ovar.get
      yield a)
      .warp
      .runIO
      .===(Outcome.Success(42))
    }

    "fork & get & cancel" >>{
      (for
        ovar <- OnceVar[Int]
        fib <- ovar.get.fork
        _ <- fib.cancel
      yield ())
      .warp
      .runIO
      .===(Outcome.Success(()))
    }
  }

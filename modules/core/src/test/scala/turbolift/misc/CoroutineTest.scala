package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.Outcome
import turbolift.io.Coroutine


class CoroutineTest extends Specification:
  sequential

  "basic" >> {
    "0 yields" >>{
      (for
        coro <- Coroutine.create[Int, String, Any]: fx =>
          !!.pure("bye")
        a <- coro.resume
      yield a)
      .runIO === Outcome.Success(Left("bye"))
    }

    "1 yield" >>{
      (for
        coro <- Coroutine.create[Int, String, Any]: fx =>
          fx.yeld(42) &&!
          !!.pure("bye")
        a <- coro.resume
        b <- coro.resume
      yield (a, b))
      .runIO === Outcome.Success((Right(42), Left("bye")))
    }

    "2 yields" >>{
      (for
        coro <- Coroutine.create[Int, String, Any]: fx =>
          fx.yeld(42) &&!
          fx.yeld(1337) &&!
          !!.pure("bye")
        a <- coro.resume
        b <- coro.resume
        c <- coro.resume
      yield (a, b, c))
      .runIO === Outcome.Success((Right(42), Right(1337), Left("bye")))
    }

    "exit" >>{
      (for
        coro <- Coroutine.create[Int, String, Any]: fx =>
          fx.exit("omg") &&!
          !!.pure("bye")
        a <- coro.resume
      yield a)
      .runIO === Outcome.Success(Left("omg"))
    }

    "yield & exit" >>{
      (for
        coro <- Coroutine.create[Int, String, Any]: fx =>
          fx.yeld(42) &&!
          fx.exit("omg") &&!
          fx.yeld(1337) &&!
          !!.pure("bye")
        a <- coro.resume
        b <- coro.resume
      yield (a, b))
      .runIO === Outcome.Success((Right(42), Left("omg")))
    }
  }

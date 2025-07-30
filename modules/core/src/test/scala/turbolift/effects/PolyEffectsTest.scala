package turbolift.effects
import org.specs2.mutable._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.{Reader, Writer, State, Error, Finalizer}
import turbolift.mode.ST


class EffectTest extends Specification:
  "Reader" >>{
    trait Foo { def foo: Int }
    trait Bar { def bar: Boolean }
    final class FooBar(override val foo: Int, override val bar: Boolean) extends Foo with Bar

    val prog =
      Reader.asks[Foo](_.foo) **!
      Reader.asks[Bar](_.bar)

    Typecheck {"""
      val _: (Int, Boolean) !! Reader.@@[Foo & Bar] = prog
    """} must succeed

    prog.handleWith(Reader.handler(FooBar(42, true))).run.===((42, true))
  }


  "Writer" >>{
    import scala.{List => Nel} // ;-)

    val prog =
      Writer.tell(Nel(42)) &&!
      Writer.tell(Nel(true))

    Typecheck {"""
      val _: Unit !! Writer.@@[Nel[Int | Boolean]] = prog
    """} must succeed

    prog.handleWith(Writer.handler[Nel[Int | Boolean]].justState).run.===(Nel(42, true))
  }


  "State" >>{
    val prog =
      (for
        a <- State.get[Int]
        _ <- State.put(a + 1)
      yield a)

    Typecheck {"""
      val _: Int !! State.@@[Int] = prog
    """} must succeed

    prog.handleWith(State.handler(42)).run.===((42, 43))
  }


  "Error" >> {
    "FO" >>{
      val prog =
        Error.raise(42).as(()) &&!
        Error.raise(true).as(())

      Typecheck {"""
        val _: Unit !! Error.@@[Int | Boolean] = prog
      """} must succeed

      prog.handleWith(Error.handler).run.===(Left(42))
    }

    "HO" >>{
      val prog =
        Error.catchToEither[Int](Error.raise(42).as(())) **!
        Error.catchToEither[Boolean](Error.raise(true).as(()))

      Typecheck {"""
        val _: (Either[Int, Unit], Either[Boolean, Unit]) !! Error.@@[Int | Boolean] = prog
      """} must succeed

      prog.handleWith(Error.handler).run.===(Right((Left(42), Left(true))))
    }
  }


  "Finalizer" >>{
    case object S1 extends StateEffect[Int]
    case object S2 extends StateEffect[Int]
    type S1 = S1.type
    type S2 = S2.type
    case object E extends Exception

    val prog =
      IO.catchToEither:
        (for
          _ <- Finalizer.register(S1.modify(_ + 1))
          _ <- Finalizer.register(S2.modify(_ + 1))
          _ <- IO(throw E)
        yield ())
      .finalized

    Typecheck {"""
      val _: Either[Throwable, Unit] !! (S1 & S2 & IO) = prog
    """} must succeed

    prog
    .handleWith(S1.handler(10))
    .handleWith(S2.handler(100))
    .runIO.toEither.===(Right(((Left(E), 11), 101)))
  }

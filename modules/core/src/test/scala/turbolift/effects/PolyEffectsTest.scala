package turbolift.effects
import org.specs2.mutable._
import org.specs2.execute.Typecheck
import org.specs2.matcher.TypecheckMatchers._
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects._
import turbolift.mode.ST


class PolyEffectTest extends Specification:
  "PolyReader" >>{
    trait Foo { def foo: Int }
    trait Bar { def bar: Boolean }
    final class FooBar(override val foo: Int, override val bar: Boolean) extends Foo with Bar

    val prog =
      PolyReader.asks[Foo](_.foo) **!
      PolyReader.asks[Bar](_.bar)

    Typecheck {"""
      val _: (Int, Boolean) !! PolyReader.@@[Foo & Bar] = prog
    """} must succeed

    prog.handleWith(PolyReader.handlers.default(FooBar(42, true))).run.===((42, true))
  }


  "PolyWriter" >>{
    import scala.{List => Nel} // ;-)

    val prog =
      PolyWriter.tell(Nel(42)) &&!
      PolyWriter.tell(Nel(true))

    Typecheck {"""
      val _: Unit !! PolyWriter.@@[Nel[Int | Boolean]] = prog
    """} must succeed

    prog.handleWith(PolyWriter.handlers.local[Nel[Int | Boolean]].justState).run.===(Nel(42, true))
  }


  "PolyState" >>{
    val prog =
      (for
        a <- PolyState.get[Int]
        _ <- PolyState.put(a + 1)
      yield a)

    Typecheck {"""
      val _: Int !! PolyState.@@[Int] = prog
    """} must succeed

    prog.handleWith(PolyState.handlers.local(42)).run.===((42, 43))
  }


  "PolyError" >> {
    "FO" >>{
      val prog =
        PolyError.raise(42).as(()) &&!
        PolyError.raise(true).as(())

      Typecheck {"""
        val _: Unit !! PolyError.@@[Int | Boolean] = prog
      """} must succeed

      prog.handleWith(PolyError.handlers.default).run.===(Left(42))
    }

    "HO" >>{
      val prog =
        PolyError.catchToEither[Int](PolyError.raise(42).as(())) **!
        PolyError.catchToEither[Boolean](PolyError.raise(true).as(()))

      Typecheck {"""
        val _: (Either[Int, Unit], Either[Boolean, Unit]) !! PolyError.@@[Int | Boolean] = prog
      """} must succeed

      prog.handleWith(PolyError.handlers.default).run.===(Right((Left(42), Left(true))))
    }
  }


  "PolyResource" >>{
    case object S1 extends State[Int]
    case object S2 extends State[Int]
    type S1 = S1.type
    type S2 = S2.type
    case object E extends Exception

    val prog =
      IO.catchToEither:
        (for
          _ <- PolyResource.register(S1.modify(_ + 1))
          _ <- PolyResource.register(S2.modify(_ + 1))
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

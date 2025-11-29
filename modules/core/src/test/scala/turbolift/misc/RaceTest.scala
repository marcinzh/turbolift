package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.{IO, WriterEffectK, ErrorEffectK, ReaderEffect, Choice}
import turbolift.data.{Outcome, Cause}
import turbolift.io.{Fiber, Zipper, AtomicVar}


class RaceTest extends Specification:
  sequential

  "basic" >> {
    "raceFibers" >> {
      def win = !!.pure(42)
      def lose = IO.sleep(10).as("BAD")
      def canc = IO.cancel

      extension [A, B](thiz: Either[(Zipper[A, IO], Fiber[B, IO]), (Fiber[A, IO], Zipper[B, IO])] !! IO)
        def justZipper: Either[Outcome[A], Outcome[B]] !! IO =
          thiz.map:
            case Left((zipp, _)) => Left(zipp.getIO)
            case Right((_, zipp)) => Right(zipp.getIO)

      "left wins" >>{
        IO.raceFibers(win, lose).warp.justZipper.runIO === Outcome.Success(Left(Outcome.Success(42)))
      }
      "right wins" >>{
        IO.raceFibers(lose, win).warp.justZipper.runIO === Outcome.Success(Right(Outcome.Success(42)))
      }
      "left cancelled" >>{
        IO.raceFibers(canc, lose).warp.justZipper.runIO === Outcome.Success(Left(Outcome.Cancelled))
      }
      "right cancelled" >>{
        IO.raceFibers(lose, canc).warp.justZipper.runIO === Outcome.Success(Right(Outcome.Cancelled))
      }
    }

    "race many" >> {
      def sleep(n: Int) = IO.sleep(n).as(n)
      case object E extends Exception { override def toString = productPrefix }

      "raceFirst" >> {
        "all succeed" >>{
          IO.raceFirst(Vector(sleep(10), sleep(1), sleep(50)))
          .runIO === Outcome.Success(1)
        }
        "all cancelled" >>{
          IO.raceFirst(Vector(IO.cancel, IO.cancel))
          .runIO === Outcome.Cancelled
        }
        "one cancelled early" >>{
          IO.raceFirst(Vector(IO.cancel, sleep(1), sleep(50)))
          .runIO === Outcome.Success(1)
        }
        "one failed early" >>{
          IO.raceFirst(Vector(sleep(10), IO(throw E)))
          .runIO === Outcome.Failure(Cause.Thrown(E))
        }
      }

      "raceAll" >> {
        "all succeed" >>{
          IO.raceAll(Vector(sleep(10), sleep(1), sleep(50)))
          .runIO === Outcome.Success(Vector(10, 1, 50))
        }
        "one cancelled" >>{
          IO.raceAll(Vector(IO.cancel, sleep(1), sleep(50)))
          .runIO === Outcome.Cancelled
        }
        "one failed" >>{
          IO.raceAll(Vector(sleep(1), IO.sleep(50) &&! IO(throw E)))
          .runIO === Outcome.Failure(Cause.Thrown(E))
        }
      }

      "raceAllVoid" >>{
        (for
          avar <- AtomicVar(0)
          a <- IO.raceAllVoid(Vector(
            avar.modify(_ + 1),
            avar.modify(_ + 10),
            avar.modify(_ + 100),
          ))
          b <- avar.get
        yield (a, b))
        .runIO === Outcome.Success(((), 111))
      }

      "edge cases" >> {
        val aa = !!.pure(42)
        "raceFirst 0" >>{ IO.raceFirst(Vector()).runIO === Outcome.Cancelled }
        "raceFirst 1" >>{ IO.raceFirst(Vector(aa)).runIO === Outcome.Success(42) }
        "raceAll 0" >>{ IO.raceAll(Vector()).runIO === Outcome.Success(Vector()) }
        "raceAll 1" >>{ IO.raceAll(Vector(aa)).runIO === Outcome.Success(Vector(42)) }
      }
    }
  }


  "effectful" >> {
    case object W extends WriterEffectK[Vector, String]
    case object E extends ErrorEffectK[Vector, String]
    case object R1 extends ReaderEffect[Boolean]
    case object R2 extends ReaderEffect[Double]

    "raceAll + Writer" >>{
      IO.raceAll(Vector(
        W.tell("a").as(1),
        W.tell("b").as(2),
        W.tell("c").as(3),
      ))
      .handleWith(R1.handler(false))
      .handleWith(W.handler)
      .handleWith(R2.handler(0.0))
      .run === (Vector(1, 2, 3), Vector("a", "b", "c"))
    }

    "raceAll + Error" >>{
      IO.raceAll(Vector(
        !!.pure(1),
        E.raise("a"),
        E.raise("b"),
        E.raise("c"),
      ))
      .handleWith(R1.handler(false))
      .handleWith(E.handlers.all)
      .handleWith(R2.handler(0.0))
      .run === Left(Vector("a", "b", "c"))
    }

    "raceAll + Choice" >>{
      IO.raceAll(Vector(
        Choice.choose(Vector(1, 2)),
        Choice.choose(Vector(10, 20)),
      ))
      .handleWith(R1.handler(false))
      .handleWith(Choice.handlers.all)
      .handleWith(R2.handler(0.0))
      .run === Vector(Vector(1, 10), Vector(1, 20), Vector(2, 10), Vector(2, 20))
    }
  }

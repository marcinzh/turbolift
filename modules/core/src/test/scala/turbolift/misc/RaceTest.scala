package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.Outcome
import turbolift.io.{Fiber, Zipper}
import Auxx._


class RaceTest extends Specification:
  sequential

  "basic" >> {
    def win = !!.pure(42)
    def lose = IO.sleep(10).as("BAD")
    def canc = IO.cancel

    extension [A, B](thiz: Either[(Zipper[A, IO], Fiber[B, IO]), (Fiber[A, IO], Zipper[B, IO])] !! IO)
      def justGetFromZippers: Either[Outcome[A], Outcome[B]] !! IO =
        thiz.map:
          case Left((z, _)) => Left(z.getIO)
          case Right((_, z)) => Right(z.getIO)

    "left wins" >>{
      IO.raceFibers(win, lose).warp.justGetFromZippers.runIO === Outcome.Success(Left(Outcome.Success(42)))
    }
    "right wins" >>{
      IO.raceFibers(lose, win).warp.justGetFromZippers.runIO === Outcome.Success(Right(Outcome.Success(42)))
    }
    "left cancelled" >>{
      IO.raceFibers(canc, lose).warp.justGetFromZippers.runIO === Outcome.Success(Left(Outcome.Cancelled))
    }
    "right cancelled" >>{
      IO.raceFibers(lose, canc).warp.justGetFromZippers.runIO === Outcome.Success(Right(Outcome.Cancelled))
    }
  }

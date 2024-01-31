package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{Outcome, Cause}
import turbolift.mode.ST


class IOTest extends Specification:
  "Basic ops" >> {
    "fail" >>{
      val e = new Exception("e")
      IO.fail(e).unsafeRun === Outcome.Failure(Cause.Thrown(e))
    }

    "cancel" >>{
      IO.cancel.unsafeRun === Outcome.Cancelled
    }

    "yield" >>{
      IO.yeld.unsafeRun === Outcome.Success(())
    }

    "yield order" >>{
      var accum = ""
      def append(s: String) = !!.impure { accum = accum ++ s }
      val prog1 =
        for
          _ <- append("1")
          _ <- IO.yeld
          _ <- append("2")
        yield ()
      val prog2 = append("3")

      (prog1 &! prog2).unsafeRun
      accum === "132"
    }
  }

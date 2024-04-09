package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{Outcome, Cause}


class BinaryOpTest extends Specification:
  sequential

  val a = 'a'
  val b = "b"
  val fx = new Exception("f")
  val gx = new Exception("g")
  val f = Cause.Thrown(fx)
  val g = Cause.Thrown(gx)
  val aw = !!.pure(a)
  val bw = !!.pure(b)
  val cw = IO.cancel
  val fw = IO.raise(fx)
  val gw = IO.raise(gx)

  def lose[A](xx: A !! IO) = IO(Thread.sleep(1)) &&! IO.yeld &&! xx
  val al = lose(aw)
  val bl = lose(bw)
  val cl = lose(cw)
  val fl = lose(fw)
  val gl = lose(gw)


  "sequential" >> {
    "**!" >> {
      "success **! success" >>{ (aw **! bw).unsafeRun === Outcome.Success((a, b)) }
      "success **! cancel"  >>{ (aw **! cw).unsafeRun === Outcome.Cancelled }
      "success **! failure" >>{ (aw **! gw).unsafeRun === Outcome.Failure(g) }
      "cancel **! success"  >>{ (cw **! bw).unsafeRun === Outcome.Cancelled }
      "cancel **! cancel"   >>{ (cw **! cw).unsafeRun === Outcome.Cancelled }
      "cancel **! failure"  >>{ (cw **! gw).unsafeRun === Outcome.Cancelled }
      "failure **! success" >>{ (fw **! bw).unsafeRun === Outcome.Failure(f) }
      "failure **! cancel"  >>{ (fw **! cw).unsafeRun === Outcome.Failure(f) }
      "failure **! failure" >>{ (fw **! gw).unsafeRun === Outcome.Failure(f) }
    }

    "&&!" >> {
      "success &&! success" >>{ (aw &&! bw).unsafeRun === Outcome.Success(b) }
      "success &&! cancel"  >>{ (aw &&! cw).unsafeRun === Outcome.Cancelled }
      "success &&! failure" >>{ (aw &&! gw).unsafeRun === Outcome.Failure(g) }
      "cancel &&! success"  >>{ (cw &&! bw).unsafeRun === Outcome.Cancelled }
      "cancel &&! cancel"   >>{ (cw &&! cw).unsafeRun === Outcome.Cancelled }
      "cancel &&! failure"  >>{ (cw &&! gw).unsafeRun === Outcome.Cancelled }
      "failure &&! success" >>{ (fw &&! bw).unsafeRun === Outcome.Failure(f) }
      "failure &&! cancel"  >>{ (fw &&! cw).unsafeRun === Outcome.Failure(f) }
      "failure &&! failure" >>{ (fw &&! gw).unsafeRun === Outcome.Failure(f) }
    }

    "||!" >> {
      "success ||! success" >>{ (aw ||! bw).unsafeRun === Outcome.Success(a) }
      "success ||! cancel"  >>{ (aw ||! cw).unsafeRun === Outcome.Success(a) }
      "success ||! failure" >>{ (aw ||! gw).unsafeRun === Outcome.Success(a) }
      "cancel ||! success"  >>{ (cw ||! bw).unsafeRun === Outcome.Success(b) }
      "cancel ||! cancel"   >>{ (cw ||! cw).unsafeRun === Outcome.Cancelled }
      "cancel ||! failure"  >>{ (cw ||! gw).unsafeRun === Outcome.Failure(g) }
      "failure ||! success" >>{ (fw ||! bw).unsafeRun === Outcome.Failure(f) }
      "failure ||! cancel"  >>{ (fw ||! cw).unsafeRun === Outcome.Failure(f) }
      "failure ||! failure" >>{ (fw ||! gw).unsafeRun === Outcome.Failure(f) }
    }
  }


  "parallel" >> {
    "&!" >> {
      "left wins" >> {
        "success &! success" >>{ (aw &! bl).unsafeRun === Outcome.Success(b) }
        "success &! cancel"  >>{ (aw &! cl).unsafeRun === Outcome.Cancelled }
        "success &! failure" >>{ (aw &! gl).unsafeRun === Outcome.Failure(g) }
        "cancel &! success"  >>{ (cw &! bl).unsafeRun === Outcome.Cancelled }
        "cancel &! cancel"   >>{ (cw &! cl).unsafeRun === Outcome.Cancelled }
        "cancel &! failure"  >>{ (cw &! gl).unsafeRun === Outcome.Cancelled }
        "failure &! success" >>{ (fw &! bl).unsafeRun === Outcome.Failure(f) }
        "failure &! cancel"  >>{ (fw &! cl).unsafeRun === Outcome.Failure(f) }
        "failure &! failure" >>{ (fw &! gl).unsafeRun === Outcome.Failure(f) }
      }

      "right wins" >> {
        "success &! success" >>{ (al &! bw).unsafeRun === Outcome.Success(b) }
        "success &! cancel"  >>{ (al &! cw).unsafeRun === Outcome.Cancelled }
        "success &! failure" >>{ (al &! gw).unsafeRun === Outcome.Failure(g) }
        "cancel &! success"  >>{ (cl &! bw).unsafeRun === Outcome.Cancelled }
        "cancel &! cancel"   >>{ (cl &! cw).unsafeRun === Outcome.Cancelled }
        "cancel &! failure"  >>{ (cl &! gw).unsafeRun === Outcome.Failure(g) }
        "failure &! success" >>{ (fl &! bw).unsafeRun === Outcome.Failure(f) }
        "failure &! cancel"  >>{ (fl &! cw).unsafeRun === Outcome.Cancelled }
        "failure &! failure" >>{ (fl &! gw).unsafeRun === Outcome.Failure(g) }
      }
    }

    "|!" >> {
      "left wins" >> {
        "success |! success" >>{ (aw |! bl).unsafeRun === Outcome.Success(a) }
        "success |! cancel"  >>{ (aw |! cl).unsafeRun === Outcome.Success(a) }
        "success |! failure" >>{ (aw |! gl).unsafeRun === Outcome.Success(a) }
        "cancel |! success"  >>{ (cw |! bl).unsafeRun === Outcome.Success(b) }
        "cancel |! cancel"   >>{ (cw |! cl).unsafeRun === Outcome.Cancelled }
        "cancel |! failure"  >>{ (cw |! gl).unsafeRun === Outcome.Failure(g) }
        "failure |! success" >>{ (fw |! bl).unsafeRun === Outcome.Failure(f) }
        "failure |! cancel"  >>{ (fw |! cl).unsafeRun === Outcome.Failure(f) }
        "failure |! failure" >>{ (fw |! gl).unsafeRun === Outcome.Failure(f) }
      }

      "right wins" >> {
        "success |! success" >>{ (al |! bw).unsafeRun === Outcome.Success(b) }
        "success |! cancel"  >>{ (al |! cw).unsafeRun === Outcome.Success(a) }
        "success |! failure" >>{ (al |! gw).unsafeRun === Outcome.Failure(g) }
        "cancel |! success"  >>{ (cl |! bw).unsafeRun === Outcome.Success(b) }
        "cancel |! cancel"   >>{ (cl |! cw).unsafeRun === Outcome.Cancelled }
        "cancel |! failure"  >>{ (cl |! gw).unsafeRun === Outcome.Failure(g) }
        "failure |! success" >>{ (fl |! bw).unsafeRun === Outcome.Success(b) }
        "failure |! cancel"  >>{ (fl |! cw).unsafeRun === Outcome.Failure(f) }
        "failure |! failure" >>{ (fl |! gw).unsafeRun === Outcome.Failure(g) }
      }
    }
  }

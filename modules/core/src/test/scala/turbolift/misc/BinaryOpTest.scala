package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.data.{Outcome, Cause}


class BinaryOpTest extends Specification:
  sequential

  val a = 'a'
  val b = "b"
  case object F extends Exception { override val toString = productPrefix }
  case object G extends Exception { override val toString = productPrefix }
  val f = Cause.Thrown(F)
  val g = Cause.Thrown(G)

  val aa = !!.pure(a)
  val bb = !!.pure(b)
  val cc = IO.cancel
  val ff = IO.raise(F)
  val gg = IO.raise(G)

  def win[A](xx: A !! IO) = IO(Thread.sleep(1)) &&! xx
  def lose[A](xx: A !! IO) = IO(Thread.sleep(20)) &&! xx

  val aw = win(aa)
  val bw = win(bb)
  val cw = win(cc)
  val fw = win(ff)
  val gw = win(gg)

  val al = lose(aa)
  val bl = lose(bb)
  val cl = lose(cc)
  val fl = lose(ff)
  val gl = lose(gg)


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
        "cancel &! failure"  >>{ (cw &! gl).unsafeRun === Outcome.Failure(g) }
        "failure &! success" >>{ (fw &! bl).unsafeRun === Outcome.Failure(f) }
        "failure &! cancel"  >>{ (fw &! cl).unsafeRun === Outcome.Failure(f) }
        "failure &! failure" >>{ (fw &! gl).unsafeRun === Outcome.Failure(f ^^ g) }
      }

      "right wins" >> {
        "success &! success" >>{ (al &! bw).unsafeRun === Outcome.Success(b) }
        "success &! cancel"  >>{ (al &! cw).unsafeRun === Outcome.Cancelled }
        "success &! failure" >>{ (al &! gw).unsafeRun === Outcome.Failure(g) }
        "cancel &! success"  >>{ (cl &! bw).unsafeRun === Outcome.Cancelled }
        "cancel &! cancel"   >>{ (cl &! cw).unsafeRun === Outcome.Cancelled }
        "cancel &! failure"  >>{ (cl &! gw).unsafeRun === Outcome.Failure(g) }
        "failure &! success" >>{ (fl &! bw).unsafeRun === Outcome.Failure(f) }
        "failure &! cancel"  >>{ (fl &! cw).unsafeRun === Outcome.Failure(f) }
        "failure &! failure" >>{ (fl &! gw).unsafeRun === Outcome.Failure(g ^^ f) }
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
        "failure |! failure" >>{ (fw |! gl).unsafeRun === Outcome.Failure(f ^^ g) }
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
        "failure |! failure" >>{ (fl |! gw).unsafeRun === Outcome.Failure(g ^^ f) }
      }
    }
  }

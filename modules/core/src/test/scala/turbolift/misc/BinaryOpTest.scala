package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.effects.IO
import turbolift.io.{Outcome, Cause}
import turbolift.mode.ST


class BinaryOpTest extends Specification:
  val a = 'a'
  val b = "b"
  val fx = new Exception("f")
  val gx = new Exception("g")
  val f = Cause.Thrown(fx)
  val g = Cause.Thrown(gx)
  val aa = !!.pure(a)
  val bb = !!.pure(b)
  val cc = IO.cancel
  val ff = IO.raise(fx)
  val gg = IO.raise(gx)


  "sequential" >> {
    "**!" >> {
      "success **! success" >>{ (aa **! bb).unsafeRun === Outcome.Success((a, b)) }
      "success **! cancel"  >>{ (aa **! cc).unsafeRun === Outcome.Cancelled }
      "success **! failure" >>{ (aa **! gg).unsafeRun === Outcome.Failure(g) }
      "cancel **! success"  >>{ (cc **! bb).unsafeRun === Outcome.Cancelled }
      "cancel **! cancel"   >>{ (cc **! cc).unsafeRun === Outcome.Cancelled }
      "cancel **! failure"  >>{ (cc **! gg).unsafeRun === Outcome.Cancelled }
      "failure **! success" >>{ (ff **! bb).unsafeRun === Outcome.Failure(f) }
      "failure **! cancel"  >>{ (ff **! cc).unsafeRun === Outcome.Failure(f) }
      "failure **! failure" >>{ (ff **! gg).unsafeRun === Outcome.Failure(f) }
    }

    "&&!" >> {
      "success &&! success" >>{ (aa &&! bb).unsafeRun === Outcome.Success(b) }
      "success &&! cancel"  >>{ (aa &&! cc).unsafeRun === Outcome.Cancelled }
      "success &&! failure" >>{ (aa &&! gg).unsafeRun === Outcome.Failure(g) }
      "cancel &&! success"  >>{ (cc &&! bb).unsafeRun === Outcome.Cancelled }
      "cancel &&! cancel"   >>{ (cc &&! cc).unsafeRun === Outcome.Cancelled }
      "cancel &&! failure"  >>{ (cc &&! gg).unsafeRun === Outcome.Cancelled }
      "failure &&! success" >>{ (ff &&! bb).unsafeRun === Outcome.Failure(f) }
      "failure &&! cancel"  >>{ (ff &&! cc).unsafeRun === Outcome.Failure(f) }
      "failure &&! failure" >>{ (ff &&! gg).unsafeRun === Outcome.Failure(f) }
    }

    "||!" >> {
      "success ||! success" >>{ (aa ||! bb).unsafeRun === Outcome.Success(a) }
      "success ||! cancel"  >>{ (aa ||! cc).unsafeRun === Outcome.Success(a) }
      "success ||! failure" >>{ (aa ||! gg).unsafeRun === Outcome.Success(a) }
      "cancel ||! success"  >>{ (cc ||! bb).unsafeRun === Outcome.Success(b) }
      "cancel ||! cancel"   >>{ (cc ||! cc).unsafeRun === Outcome.Cancelled }
      "cancel ||! failure"  >>{ (cc ||! gg).unsafeRun === Outcome.Failure(g) }
      "failure ||! success" >>{ (ff ||! bb).unsafeRun === Outcome.Failure(f) }
      "failure ||! cancel"  >>{ (ff ||! cc).unsafeRun === Outcome.Failure(f) }
      "failure ||! failure" >>{ (ff ||! gg).unsafeRun === Outcome.Failure(f) }
    }
  }


  "parallel" >> {
    val ay = IO.yeld.flatMap(_ => aa)
    val cy = IO.yeld.flatMap(_ => cc)
    val fy = IO.yeld.flatMap(_ => ff)

    "*!" >> {
      "left -> right" >> {
        "success *! success" >>{ (aa *! bb).unsafeRun === Outcome.Success((a, b)) }
        "success *! cancel"  >>{ (aa *! cc).unsafeRun === Outcome.Cancelled }
        "success *! failure" >>{ (aa *! gg).unsafeRun === Outcome.Failure(g) }
        "cancel *! success"  >>{ (cc *! bb).unsafeRun === Outcome.Cancelled }
        "cancel *! cancel"   >>{ (cc *! cc).unsafeRun === Outcome.Cancelled }
        "cancel *! failure"  >>{ (cc *! gg).unsafeRun === Outcome.Cancelled }
        "failure *! success" >>{ (ff *! bb).unsafeRun === Outcome.Failure(f) }
        "failure *! cancel"  >>{ (ff *! cc).unsafeRun === Outcome.Failure(f) }
        "failure *! failure" >>{ (ff *! gg).unsafeRun === Outcome.Failure(f) }
      }

      "left <- right" >> {
        "success *! success" >>{ (ay *! bb).unsafeRun === Outcome.Success((a, b)) }
        "success *! cancel"  >>{ (ay *! cc).unsafeRun === Outcome.Cancelled }
        "success *! failure" >>{ (ay *! gg).unsafeRun === Outcome.Failure(g) }
        "cancel *! success"  >>{ (cy *! bb).unsafeRun === Outcome.Cancelled }
        "cancel *! cancel"   >>{ (cy *! cc).unsafeRun === Outcome.Cancelled }
        "cancel *! failure"  >>{ (cy *! gg).unsafeRun === Outcome.Failure(g) }
        "failure *! success" >>{ (fy *! bb).unsafeRun === Outcome.Failure(f) }
        "failure *! cancel"  >>{ (fy *! cc).unsafeRun === Outcome.Cancelled }
        "failure *! failure" >>{ (fy *! gg).unsafeRun === Outcome.Failure(g) }
      }
    }

    "&!" >> {
      "left -> right" >> {
        "success &! success" >>{ (aa &! bb).unsafeRun === Outcome.Success(b) }
        "success &! cancel"  >>{ (aa &! cc).unsafeRun === Outcome.Cancelled }
        "success &! failure" >>{ (aa &! gg).unsafeRun === Outcome.Failure(g) }
        "cancel &! success"  >>{ (cc &! bb).unsafeRun === Outcome.Cancelled }
        "cancel &! cancel"   >>{ (cc &! cc).unsafeRun === Outcome.Cancelled }
        "cancel &! failure"  >>{ (cc &! gg).unsafeRun === Outcome.Cancelled }
        "failure &! success" >>{ (ff &! bb).unsafeRun === Outcome.Failure(f) }
        "failure &! cancel"  >>{ (ff &! cc).unsafeRun === Outcome.Failure(f) }
        "failure &! failure" >>{ (ff &! gg).unsafeRun === Outcome.Failure(f) }
      }

      "left <- right" >> {
        "success &! success" >>{ (ay &! bb).unsafeRun === Outcome.Success(b) }
        "success &! cancel"  >>{ (ay &! cc).unsafeRun === Outcome.Cancelled }
        "success &! failure" >>{ (ay &! gg).unsafeRun === Outcome.Failure(g) }
        "cancel &! success"  >>{ (cy &! bb).unsafeRun === Outcome.Cancelled }
        "cancel &! cancel"   >>{ (cy &! cc).unsafeRun === Outcome.Cancelled }
        "cancel &! failure"  >>{ (cy &! gg).unsafeRun === Outcome.Failure(g) }
        "failure &! success" >>{ (fy &! bb).unsafeRun === Outcome.Failure(f) }
        "failure &! cancel"  >>{ (fy &! cc).unsafeRun === Outcome.Cancelled }
        "failure &! failure" >>{ (fy &! gg).unsafeRun === Outcome.Failure(g) }
      }
    }

    "|!" >> {
      "left -> right" >> {
        "success |! success" >>{ (aa |! bb).unsafeRun === Outcome.Success(a) }
        "success |! cancel"  >>{ (aa |! cc).unsafeRun === Outcome.Success(a) }
        "success |! failure" >>{ (aa |! gg).unsafeRun === Outcome.Success(a) }
        "cancel |! success"  >>{ (cc |! bb).unsafeRun === Outcome.Success(b) }
        "cancel |! cancel"   >>{ (cc |! cc).unsafeRun === Outcome.Cancelled }
        "cancel |! failure"  >>{ (cc |! gg).unsafeRun === Outcome.Failure(g) }
        "failure |! success" >>{ (ff |! bb).unsafeRun === Outcome.Failure(f) }
        "failure |! cancel"  >>{ (ff |! cc).unsafeRun === Outcome.Failure(f) }
        "failure |! failure" >>{ (ff |! gg).unsafeRun === Outcome.Failure(f) }
      }

      "left <- right" >> {
        "success |! success" >>{ (ay |! bb).unsafeRun === Outcome.Success(b) }
        "success |! cancel"  >>{ (ay |! cc).unsafeRun === Outcome.Success(a) }
        "success |! failure" >>{ (ay |! gg).unsafeRun === Outcome.Failure(g) }
        "cancel |! success"  >>{ (cy |! bb).unsafeRun === Outcome.Success(b) }
        "cancel |! cancel"   >>{ (cy |! cc).unsafeRun === Outcome.Cancelled }
        "cancel |! failure"  >>{ (cy |! gg).unsafeRun === Outcome.Failure(g) }
        "failure |! success" >>{ (fy |! bb).unsafeRun === Outcome.Success(b) }
        "failure |! cancel"  >>{ (fy |! cc).unsafeRun === Outcome.Failure(f) }
        "failure |! failure" >>{ (fy |! gg).unsafeRun === Outcome.Failure(g) }
      }
    }
  }


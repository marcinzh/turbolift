package turbolift.misc
import org.specs2.mutable._
import turbolift.!!
import turbolift.mode.ST


class ExceptionTest extends Specification:
  def bad = !!.impure(throw Error("bad"))
  def ook = !!.pure(42)

  "Basic ops" >> {
    bad.unsafeRun.isFailure === true
  }

  "Par ops" >> {
    (bad *! ook).unsafeRun.isFailure === true
    (ook *! bad).unsafeRun.isFailure === true
    (bad *! bad).unsafeRun.isFailure === true
  }

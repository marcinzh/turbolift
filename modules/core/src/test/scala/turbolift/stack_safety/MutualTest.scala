package turbolift.stack_safety
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.!!


class MutualTest extends AnyFunSpec with CanStackOverflow:
  describe("Mutually recursive tail calls using should be stack safe") {
    def isEven(xs: List[Int]): Boolean !! Any =
      if xs.isEmpty
      then !!.pure(true)
      else !!.defer(isOdd(xs.tail))

    def isOdd(xs: List[Int]): Boolean !! Any =
      if xs.isEmpty
      then !!.pure(false)
      else !!.defer(isEven(xs.tail))

    mustNotStackOverflow {
      isEven((1 to TooBigForStack).toList).run
    }
  }

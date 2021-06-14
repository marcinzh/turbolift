package turbolift.stack_safety
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!


class MutualTest extends AnyFlatSpec with CanStackOverflow:
  "Mutually recursive tail calls using `defer`" should "be stack safe" in {
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

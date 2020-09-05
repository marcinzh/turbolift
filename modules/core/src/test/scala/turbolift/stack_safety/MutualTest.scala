package turbolift.stack_safety
import turbolift.abstraction.!!
import org.specs2._


class MutualTest extends Specification with CanStackOverflow {
  def is = evenOdd

  def evenOdd = br ^ "Mutually recursive tail calls using `defer` should be stack safe" ! {
    def isEven(xs: List[Int]): Boolean !! Any =
      if (xs.isEmpty) !!.pure(true) else !!.defer { isOdd(xs.tail) }

    def isOdd(xs: List[Int]): Boolean !! Any =
      if (xs.isEmpty) !!.pure(false) else !!.defer { isEven(xs.tail) }

    mustNotStackOverflow {
      isEven((1 to TooBigForStack).toList).run
    }
  }
}

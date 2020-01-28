package turbolift.stack_safety
import turbolift.abstraction.!!
import turbolift.std_effects.Memoizer
import org.specs2._


class RecursiveMemoTest extends Specification with CanStackOverflow {
  def is = recmemo

  def recmemo = {
    case object FxMemo extends Memoizer[Int, Int]

    def foo(n: Int): Int !! FxMemo.type =
      if (n > 0)
        for {
          x <- FxMemo.memo(foo)(n - 1)
        } yield x + 1
      else
        !!.pure(1)

    mustNotStackOverflow { foo(TooBigForStack).runWith(FxMemo.handler) }
  }
}

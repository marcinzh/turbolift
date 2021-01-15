package turbolift.stack_safety
import turbolift.abstraction.!!
import turbolift.std_effects.Reader
import turbolift.extra_effects.{AcyclicMemoizer, CyclicMemoizer}
import org.specs2._


class RecursiveMemoTest extends Specification with CanStackOverflow {
  def is = List(acyclic, cyclic).reduce(_ ^ _)

  def acyclic = br ^ "acyclic" ! {
    case object FxMemo extends AcyclicMemoizer[Int, Int]

    object LetRec {
      val recur = FxMemo(foo)(_)
      def foo(n: Int): Int !! FxMemo.type =
        if (n > 0)
          for {
            x <- recur(n - 1)
          } yield x + 1
        else
          !!.pure(1)
    }

    mustNotStackOverflow {
      LetRec.recur(TooBigForStack)
      .&&!(FxMemo.get)
      .runWith(FxMemo.handler)
    }
  }

  case class Node(
    label: Int,
    prev: () => Node,
    next: () => Node,
  )

  def cyclic = br ^ "cyclic" ! {
    case object FxMemo extends CyclicMemoizer[Int, Node]

    object LetRec {
      val recur = FxMemo(foo(TooBigForStack))(_)
      def foo(m: Int)(n: Int): Node !! FxMemo.type =
        for {
          prev <- recur(if (n > 0) n else m)
          next <- recur(if (n < m) n else 0)
          node = Node(label = n, prev = prev, next = next) 
        } yield node
    }

    mustNotStackOverflow {
      LetRec.recur(0)
      .&&!(FxMemo.get)
      .runWith(FxMemo.handler)
    }
  }
}

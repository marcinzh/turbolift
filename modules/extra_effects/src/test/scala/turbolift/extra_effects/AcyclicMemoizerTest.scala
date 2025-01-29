package turbolift.extra_effects
import org.specs2.mutable._
import org.specs2.execute.Result
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.WriterK
import turbolift.effects.AcyclicMemoizer
import turbolift.mode.ST


class AcyclicMemoizerTest extends Specification with CanLaunchTheMissiles:
  "Memoizing recursive function" >> {
    def prog(n: Int) =
      val missiles = Missile.make(n + 1)

      case object M extends AcyclicMemoizer[Int, Int]
      type M = M.type

      def fib(i: Int) =
        missiles(i).launch_! &&! (
          if i <= 1 then
            !!.pure(i)
          else
            (M.memo(i - 1) *! M.memo(i - 2)).map(_ + _)
        )

      (for
        a <- M.memo(n)
        b <- M.memo(n - 1)
        c <- M.memo(n - 2)
      yield ((c, b, a), missiles))
      .handleWith(M.handler(fib))

    val (results, missiles) = prog(10).run
    results === ((21, 34, 55))
    Result.foreach(missiles)(_.mustHaveLaunchedOnce)
  }


  "Memoizing acyclic graph" >> {
    case object W extends WriterK[Vector, Int]
    type W = W.type

    case class Vertex(serno: Int, outgoing: List[Edge])
    case class Edge(to: Vertex)

    val outgoings = Vector(
      /*0*/ List(1,2,3,4,5),
      /*1*/ List(2,6,7),
      /*2*/ List(5,6),
      /*3*/ List(6,7),
      /*4*/ List(),
      /*5*/ List(6),
      /*6*/ List(),
      /*7*/ List()
    )

    val prog =
      val missiles = Missile.make(outgoings.size)

      case object M extends AcyclicMemoizer[Int, Vertex]
      type M = M.type

      def visit(n: Int) =
        for
          _ <- missiles(n).launch_!
          _ <- W.tell(n)
          edges <- (
            for i <- outgoings(n) yield
              for to <- M.memo(i) yield
                Edge(to)
          ).traverse
        yield Vertex(n, edges)

      (for
        v0 <- M.memo(0)
        v1 <- M.memo(5)
      yield (v0.serno, v1.serno))
      .handleWith(M.handler(visit))
      .handleWith(W.handler.mapState(_.sorted))
      .map((_, missiles))

    val (results, missiles) = prog.run
    results.===((0, 5), (0 until outgoings.size))
    Result.foreach(missiles)(_.mustHaveLaunchedOnce)
  }

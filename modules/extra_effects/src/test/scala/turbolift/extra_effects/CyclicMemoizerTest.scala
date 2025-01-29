package turbolift.extra_effects
import org.specs2.mutable._
import org.specs2.execute.Result
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.WriterK
import turbolift.effects.CyclicMemoizer
import turbolift.mode.ST


class CyclicMemoizerTest extends Specification with CanLaunchTheMissiles:
  "Memoizing cyclic graph" >> {
    case object W extends WriterK[Vector, Int]
    type W = W.type

    case class Vertex(serno: Int, outgoing: List[Edge])
    case class Edge(from: () => Vertex, to: () => Vertex)

    val outgoings = Vector(
      /*0*/ List(0,1,2,3,4,5),
      /*1*/ List(6,7),
      /*2*/ List(7,2,1),
      /*3*/ List(3,7),
      /*4*/ List(),
      /*5*/ List(6),
      /*6*/ List(0),
      /*7*/ List()
    )

    val prog =
      val missiles = Missile.make(outgoings.size)

      case object M extends CyclicMemoizer[Int, Vertex]
      type M = M.type

      def visit(n: Int) =
        for
          _ <- missiles(n).launch_!
          _ <- W.tell(n)
          from <- M.memo(n)
          edges <- (
            for i <- outgoings(n) yield
              for to <- M.memo(i) yield
                Edge(from, to)
          ).traverse
        yield Vertex(n, edges)

      (for
        v0 <- M.memo(0)
        v1 <- M.memo(5)
      yield (v0().serno, v1().serno))
      .handleWith(M.handler(visit))
      .handleWith(W.handler.mapState(_.sorted))
      .map((_, missiles))

    val (results, missiles) = prog.run
    results.===((0, 5), (0 until outgoings.size))
    Result.foreach(missiles)(_.mustHaveLaunchedOnce)
  }

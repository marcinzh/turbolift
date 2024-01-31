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
    case object M extends CyclicMemoizer[Int, Vertex]
    case object W extends WriterK[Vector, Int]

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
      for
        missiles <- !!.impure(Missile.make(outgoings.size))

        visit = M.fix[W.type] { recur => n =>
          for
            _ <- missiles(n).launch_!
            _ <- W.tell(n)
            from <- recur(n)
            edges <- (
              for i <- outgoings(n) yield
                for to <- recur(i) yield
                  Edge(from, to)
            ).traverse
          yield Vertex(n, edges)
        }

        _ <- visit(0)
      yield missiles

    val (missiles, log) = prog.handleWith(M.handler).handleWith(W.handler).run
    log.sorted === (0 until outgoings.size)
    
    Result.foreach(missiles)(_.mustHaveLaunchedOnce)
  }

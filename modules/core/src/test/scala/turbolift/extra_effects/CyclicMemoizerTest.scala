package turbolift.extra_effects
import cats.implicits._
import org.specs2._
import turbolift.abstraction.!!
import turbolift.abstraction.Implicits._
import turbolift.std_effects.WriterK
import turbolift.extra_effects.CyclicMemoizer
import turbolift.operations.CanLaunchTheMissiles


class CyclicMemoizerTest extends Specification with CanLaunchTheMissiles {
  def is = graphTest

  def graphTest = br ^ "graph" ! {
    case object FxMemo extends CyclicMemoizer[Int, Vertex]
    case object FxLog extends WriterK[Vector, Int]

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

    val missiles = outgoings.map(_ => Missile())

    val visit = FxMemo.fix[FxMemo.type with FxLog.type] { recur => n =>
      for {
        _ <- missiles(n).launch_!
        _ <- FxLog.tell(n)
        from <- recur(n)
        edges <- (
          for (i <- outgoings(n))
            yield for (to <- recur(i))
              yield Edge(from, to)
        ).traverse
      } yield Vertex(n, edges)
    }

    val (log, roots) = visit(0).runWith(FxLog.handler <<<! FxMemo.handler)

    missiles.map(_.mustHaveLaunchedOnce).reduce(_ and _) and
    (log.sorted must_== (0 until outgoings.size))
  }
}

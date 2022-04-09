package turbolift.extra_effects
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.!!
import turbolift.Implicits._
import turbolift.std_effects.WriterK
import turbolift.extra_effects.CyclicMemoizer
import turbolift.std_effects.CanLaunchTheMissiles


class CyclicMemoizerTest extends AnyFunSpec with CanLaunchTheMissiles:
  describe("Memoizing cyclic graph") {
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
      for
        _ <- missiles(n).launch_!
        _ <- FxLog.tell(n)
        from <- recur(n)
        edges <- (
          for (i <- outgoings(n))
            yield for (to <- recur(i))
              yield Edge(from, to)
        ).traverse
      yield Vertex(n, edges)
    }

    val (log, roots) = visit(0).runWith(FxLog.handler <<<! FxMemo.handler)

    missiles.foreach(_.mustHaveLaunchedOnce)
    log.sorted shouldEqual (0 until outgoings.size)
  }

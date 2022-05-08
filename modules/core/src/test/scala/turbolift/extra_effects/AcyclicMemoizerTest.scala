package turbolift.extra_effects
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.!!
import turbolift.Implicits._
import turbolift.std_effects.WriterK
import turbolift.extra_effects.AcyclicMemoizer
import turbolift.std_effects.CanLaunchTheMissiles


class AcyclicMemoizerTest extends AnyFunSpec with CanLaunchTheMissiles:

  describe("Memoizing recursive function") {
    def runFibs(n: Int): (Int, Vector[Missile]) =
      val missiles = Vector.fill(n + 1)(Missile())
      
      case object FxMemo extends AcyclicMemoizer[Int, Int]

      val fib = FxMemo.fix { recur => i =>
        missiles(i).launch_! &&! (
          if (i <= 1) 
            !!.pure(i)
          else
            for {
              a <- recur(i - 1)
              b <- recur(i - 2)
              c = a + b
            } yield c
        )
      }

      (fib(n).runWith(FxMemo.handler), missiles)

    val (n, missiles) = runFibs(10)
    n shouldEqual 55
    missiles.foreach(_.mustHaveLaunchedOnce)
  }


  describe("Memoizing acyclic graph") {
    case object FxMemo extends AcyclicMemoizer[Int, Vertex]
    case object FxLog extends WriterK[Vector, Int]

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

    val missiles = outgoings.map(_ => Missile())

    val visit = FxMemo.fix[FxMemo.type with FxLog.type] { recur => n =>
      for
        _ <- missiles(n).launch_!
        _ <- FxLog.tell(n)
        edges <- (
          for (i <- outgoings(n))
            yield for (to <- recur(i))
              yield Edge(to)
        ).traverse
      yield Vertex(n, edges)
    }

    val (roots, log) = visit(0).runWith(FxMemo.handler &&&! FxLog.handler)

    missiles.foreach(_.mustHaveLaunchedOnce)
    log.sorted shouldEqual (0 until outgoings.size)
  }

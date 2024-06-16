package turbolift.extra_effects
import org.specs2.mutable._
import org.specs2.execute.Result
import turbolift.!!
import turbolift.Extensions._
import turbolift.effects.WriterK
import turbolift.effects.AcyclicMemoizer
import turbolift.mode.ST


class AcyclicMemoizerTest2 extends Specification with CanLaunchTheMissiles:
  "Memoizing recursive function" >> {
    def prog(n: Int) =
      val missiles = Missile.make(n + 1)

      val fib = AcyclicMemoizer.fix[Int, Int, Any] { recur => i =>
        missiles(i).launch_! &&! (
          if i <= 1 then
            !!.pure(i)
          else
            (recur(i - 1) *! recur(i - 2)).map(_ + _)
        )
      }

      (for
        a <- fib(n)
        b <- fib(n - 1)
        c <- fib(n - 2)
      yield ((c, b, a), missiles))
      .handleWith(fib.handler)

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

      val visit = AcyclicMemoizer.fix[Int, Vertex, W] { recur => n =>
        for
          _ <- missiles(n).launch_!
          _ <- W.tell(n)
          edges <- (
            for i <- outgoings(n) yield
              for to <- recur(i) yield
                Edge(to)
          ).traverse
        yield Vertex(n, edges)
      }

      (for
        v0 <- visit(0)
        v1 <- visit(5)
      yield (v0.serno, v1.serno))
      .handleWith(visit.handler)
      .handleWith(W.handler.mapState(_.sorted))
      .map((_, missiles))

    val (results, missiles) = prog.run
    results.===((0, 5), (0 until outgoings.size))
    Result.foreach(missiles)(_.mustHaveLaunchedOnce)
  }

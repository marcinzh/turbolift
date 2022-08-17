package turbolift.extra_effects
import org.specs2.mutable._
import org.specs2.execute.Result
import turbolift.!!
import turbolift.Extensions._
import turbolift.std_effects.WriterK
import turbolift.extra_effects.AcyclicMemoizer
import turbolift.std_effects.CanLaunchTheMissiles
import turbolift.mode.ST


class AcyclicMemoizerTest extends Specification with CanLaunchTheMissiles:
  "Memoizing recursive function" >> {
    case object M extends AcyclicMemoizer[Int, Int]

    def prog(n: Int): (Int, Vector[Missile]) !! M.type =
      for
        missiles <- !!.impure(Missile.make(n + 1))
        fib = M.fix { recur => i =>
          missiles(i).launch_! &&! (
            if i <= 1 then
              !!.pure(i)
            else
              (recur(i - 1) *! recur(i - 2)).map(_ + _)
          )
        }
        x <- fib(n)
      yield (x, missiles)

    val (result, missiles) = prog(10).handleWith(M.handler).run
    result === 55

    Result.foreach(missiles)(_.mustHaveLaunchedOnce)
  }


  "Memoizing acyclic graph" >> {
    case object M extends AcyclicMemoizer[Int, Vertex]
    case object W extends WriterK[Vector, Int]

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
      for
        missiles <- !!.impure(Missile.make(outgoings.size))

        visit = M.fix[W.type] { recur => n =>
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

        _ <- visit(0)
      yield missiles

    val (missiles, log) = prog.handleWith(M.handler).handleWith(W.handler).run
    log.sorted === (0 until outgoings.size)
    
    Result.foreach(missiles)(_.mustHaveLaunchedOnce)
  }

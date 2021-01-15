package turbolift.extra_effects
import cats.implicits._
import org.specs2._
import turbolift.abstraction.!!
import turbolift.abstraction.Implicits._
import turbolift.std_effects.WriterK
import turbolift.extra_effects.AcyclicMemoizer
import turbolift.operations.CanLaunchTheMissiles


class AcyclicMemoizerTest extends Specification with CanLaunchTheMissiles {
  def is = List(fibTest, graphTest).reduce(_^_)

  def fibTest = br ^ "fib" ! {
    def runFibs(n: Int): (Int, Vector[Missile]) = {
      val missiles = Vector.fill(n + 1)(Missile())
      
      case object FxMemo extends AcyclicMemoizer[Int, Int]

      def fib(i: Int): Int !! FxMemo.type =
        missiles(i).launch_! &&! (
          if (i <= 1) 
            !!.pure(i)
          else
            for {
              a <- FxMemo(fib)(i - 1) 
              b <- FxMemo(fib)(i - 2)
              c = a + b
            } yield c
        )

      (fib(n).runWith(FxMemo.handler), missiles)
    }

    val (n, missiles) = runFibs(10)
    (n must_== 55) and 
    missiles.map(_.mustHaveLaunchedOnce).reduce(_ and _)
  }


  def graphTest = br ^ "graph" ! {
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


    def visit(n: Int): Vertex !! FxMemo.type with FxLog.type = {
      for {
        _ <- missiles(n).launch_!
        _ <- FxLog.tell(n)
        edges <- (
          for (i <- outgoings(n))
            yield for (to <- FxMemo(visit)(i))
              yield Edge(to)
        ).traverse
      } yield Vertex(n, edges)
    }

    val (log, roots) = FxMemo(visit)(0).runWith(FxLog.handler <<<! FxMemo.handler)

    missiles.map(_.mustHaveLaunchedOnce).reduce(_ and _) and
    (log.sorted must_== (0 until outgoings.size))
  }
}

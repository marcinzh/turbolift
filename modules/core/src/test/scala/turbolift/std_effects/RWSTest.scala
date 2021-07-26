package turbolift.std_effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import turbolift.abstraction.!!
import turbolift.std_effects.{Reader, Writer, State}
import ReaderWriterStateHandler.Syntax._


class RWSTest extends AnyFlatSpec:
  "RWS" should "work" in {
    case object FxR extends Reader[Int]
    case object FxW extends Writer[String]
    case object FxS extends State[Double]

    val comp = for
      r <- FxR.ask
      _ <- FxW.tell(r.toString)
      s <- FxS.get
      _ <- FxW.tell(s.toString)
      _ <- FxS.put(s + r)
    yield s


    val result = (("100.5", 10.5), 0.5)

    comp
      .handleWith(FxS.handler(0.5))
      .handleWith(FxW.handler)
      .handleWith(FxR.handler(10))
      .map { case (a, (b, c)) => ((a, b), c) }
      .run
      .shouldEqual(result)

    comp
      .handleWith((FxR &! FxW &! FxS).handler(10, 0.5))
      .run
      .shouldEqual(result)
  }
